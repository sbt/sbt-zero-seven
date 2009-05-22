/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package sbt

import java.util.concurrent.LinkedBlockingQueue
import scala.collection.{immutable, mutable}
import immutable.TreeSet

/** Interface to the Distributor/Scheduler system for running tasks with dependencies described by a directed acyclic graph.*/
object ParallelRunner
{
	/** Executes work for nodes in an acyclic directed graph with root node `node`.  The name of a node is provided
	* by the `name` function, the work to perform for a node by `action`, and the logger to use for a node by `log`.
	* The maximum number of tasks to execute simultaneously is `maximumTasks`. */
	def run[D <: Dag[D]](node: D, name: D => String, action: D => Option[String], maximumTasks: Int, log: D => Logger): List[WorkFailure[D]] =
	{
		val info = DagInfo(node)
		// Create a strategy that gives each node a uniform self cost and uses the maximum cost to execute it and the nodes that depend on it
		// to determine which node to run.  The self cost could be modified to include more information about a node, such as the size of input files
		val strategy = MaxPathStrategy((d: D) => 1, info)
		val jobScheduler = DagScheduler(info, strategy)
		val distributor = new Distributor(jobScheduler, action, maximumTasks, log)
		val result = distributor.run().toList
		for( WorkFailure(work, message) <- result ) yield WorkFailure(work, "Error running " + name(work) + ": " + message)
	}
}
final class Distributor[D](scheduler: Scheduler[D], doWork: D => Option[String], workers: Int, log: D => Logger) extends NotNull
{
	require(workers > 0)
	/** The number of threads currently running. */
	private[this] var running = 0
	/** Pending notifications of completed work. */
	private[this] val complete = new java.util.concurrent.LinkedBlockingQueue[Done]
	
	final def run(): Iterable[WorkFailure[D]]  = run(scheduler)
	private[this] def run(scheduler: Scheduler[D]): Iterable[WorkFailure[D]] =
	{
		val nextScheduler = next(scheduler)
		if(isIdle && !nextScheduler.hasPending) // test if all work is complete
			nextScheduler.failures
		else
			run(waitForCompletedWork(nextScheduler))
	}
	private def atMaximum = running >= workers
	private def availableWorkers = Math.max(workers - running, 0)
	private def isIdle = running == 0
	private def next(scheduler: Scheduler[D]) =
	{
		if(atMaximum) // all resources full, do nothing
			scheduler
		else if(scheduler.hasPending)
		{
			val available = availableWorkers
			assume(available > 0)
			val (nextWork, newScheduler) = scheduler.next(available)
			val nextSize = nextWork.size
			assume(nextSize <= available)
			if(nextSize <= 0)
				assume(!isIdle)
			else
				nextWork.foreach(process)
			newScheduler
		}
		else // either all work is complete or the scheduler is waiting for current work to complete
			scheduler
	}
	// wait on the blocking queue `complete` until some work finishes
	private def waitForCompletedWork(scheduler: Scheduler[D]) =
	{
		val done = complete.take()
		running -= 1
		scheduler.complete(done.data, done.result)
	}
	private def process(data: D)
	{
		running += 1
		new Worker(data).start()
	}
	private class Worker(data: D) extends Thread with NotNull
	{
		override def run()
		{
			val result = Control.trapUnit("", log(data))(doWork(data))
			complete.put( new Done(result, data) )
		}
	}
	private class Done(val result: Option[String], val data: D) extends NotNull
}
final case class WorkFailure[D](work: D, message: String) extends NotNull
{
	override def toString = message
}
/** Schedules work of type D.  This is a mutable-style interface that should only be used only once per instance.*/
trait Scheduler[D] extends NotNull
{
	/** Notifies this scheduler that work has completed with the given result (Some with the error message or None if the work succeeded).*/
	def complete(d: D, result: Option[String]): Scheduler[D]
	/** Returns true if there is any more work to be done, although remaining work can be blocked
	* waiting for currently running work to complete.*/
	def hasPending: Boolean
	/** Returns up to 'max' units of work.  `max` is always positive.  The returned sequence cannot be empty if there is
	* no work currently being processed.*/
	def next(max: Int): (Seq[D], Scheduler[D])
	/** A list of failures that occurred to this point, as reported to the `complete` method. */
	def failures: Iterable[WorkFailure[D]]
}
private trait ScheduleStrategy[D] extends NotNull
{
	/** Adds the given work to the list of work that is ready to run.*/
	def workReady(dep: D): ScheduleStrategy[D]
	/** Returns true if there is work ready to be run. */
	def hasReady: Boolean
	/** Provides up to `max` units of work.  `max` is always positive and this method is not called
	* if hasReady is false. */
	def next(max: Int): (List[D], ScheduleStrategy[D])
}

private object DagScheduler
{
	def apply[D <: Dag[D]](info: DagInfo[D], strategy: ScheduleStrategy[D]): Scheduler[D] =
	{
		// find nodes that are ready to be run (no dependencies)
		val startReady = for( (key, value) <- info.remainingDeps if(value.isEmpty)) yield key
		val newInfo = info.withRemaining(info.remainingDeps -- startReady)
		// initialize the strategy with these starting nodes
		val newStrategy = (strategy /: startReady)( _ workReady _)
		new DagScheduler(newInfo, newStrategy, Nil)
	}
}
/** A scheduler for nodes of a directed-acyclic graph.  It requires the root of the graph
* and a strategy to select which available nodes to run on limited resources.*/
private[sbt] final class DagScheduler[D <: Dag[D]](info: DagInfo[D], strategy: ScheduleStrategy[D], val failures: List[WorkFailure[D]]) extends Scheduler[D]
{
	def next(max: Int): (Seq[D], Scheduler[D]) =
	{
		assume(max > 0)
		if(strategy.hasReady)
		{
			val (nextWork, newStrategy) = strategy.next(max)
			(nextWork, new DagScheduler(info, newStrategy, failures))
		}
		else
			(Nil, this)
	}
	def complete(work: D, result: Option[String]): Scheduler[D] =
	{
		result match
		{
			case None =>
				val (newStrategy, newInfo) = info.complete(strategy, work)( _ workReady _)
				new DagScheduler(newInfo, newStrategy, failures)
			case Some(errorMessage) =>
				new DagScheduler( info.clear(work), strategy, WorkFailure(work, errorMessage) :: failures)
		}
	}
	// the strategy might not have any work ready if the remaining work needs currently executing work to finish first
	def hasPending = strategy.hasReady || !info.remainingDeps.isEmpty
}
private object MaxPathStrategy
{
	def apply[D <: Dag[D]](selfCost: D => Int, info: DagInfo[D]): ScheduleStrategy[D] =
	{
		val cost = // compute the cost of the longest execution path ending at each node
		{
			val cost = new mutable.HashMap[D, Int]
			def computeCost(work: D): Int = info.reverseDeps.getOrElse(work, Nil).foldLeft(0)(_ max getCost(_)) + selfCost(work)
			def getCost(work: D): Int = cost.getOrElseUpdate(work, computeCost(work))
			info.remainingDeps.keys.foreach(getCost)
			cost.readOnly
		}
		// create a function to compare units of work.  This is not as simple as cost(a) compare cost(b) because it cannot return 0 for
		// unequal nodes
		implicit val compare =
			(a: D) => new Ordered[D]
			{
				def compare(b: D) =
				{
					val base = cost(a) compare cost(b)
					if(base == 0)
						a.hashCode compare b.hashCode // this is required because TreeSet interprets 0 as equal
					else
						base
				}
			}
		new OrderedStrategy(new TreeSet()(compare))
	}
}
/** A strategy that adds work to a tree and selects the last key as the next work to be done. */
private class OrderedStrategy[D](ready: TreeSet[D]) extends ScheduleStrategy[D]
{
	def next(max: Int): (List[D], ScheduleStrategy[D]) =
	{
		require(max > 0)
		assume(!ready.isEmpty)
		def nextImpl(remaining: Int, readyRemaining: TreeSet[D])(accumulate: List[D]): (List[D], ScheduleStrategy[D]) =
		{
			if(remaining <= 0 || readyRemaining.isEmpty)
			{
				assume(accumulate.size > 0)
				assume(accumulate.size + readyRemaining.size == ready.size)
				(accumulate, new OrderedStrategy(readyRemaining))
			}
			else
			{
				val next = readyRemaining.lastKey
				nextImpl(remaining - 1, readyRemaining - next)(next :: accumulate)
			}
		}
		nextImpl(max, ready)(Nil)
	}
	def workReady(dep: D) = new OrderedStrategy(ready + dep)
	def hasReady = !ready.isEmpty
}
/** A class that represents state for a DagScheduler and that MaxPathStrategy uses to initialize an OrderedStrategy. */
private final class DagInfo[D <: Dag[D]](val remainingDeps: immutable.Map[D, immutable.Set[D]],
	val reverseDeps: immutable.Map[D, immutable.Set[D]]) extends NotNull
{
	def withRemaining(newRemaining: immutable.Map[D, immutable.Set[D]]) = new DagInfo(newRemaining, reverseDeps)
	def withReverse(newReverse: immutable.Map[D, immutable.Set[D]]) = new DagInfo(remainingDeps, newReverse)
	
	/** Called when work does not complete properly and so all work that (transitively) depends on the work 
	* must be removed from the maps. */
	def clear(work: D): DagInfo[D] =
	{
		val newInfo = new DagInfo(remainingDeps - work, reverseDeps - work)
		val removed = reverseDeps.getOrElse(work, immutable.Set.empty)
		(newInfo /: removed)(_ clear _)
	}
	/** Called when work completes properly.  `initial` and `ready` are used for a fold over
	* the work that is now ready to go (becaues it was only waiting for `work` to complete).*/
	def complete[T](initial: T, work: D)(ready: (T,D) => T): (T, DagInfo[D]) =
	{
		class State(val value: T, val remainingDeps: immutable.Map[D, immutable.Set[D]]) extends NotNull
		def completed(state: State, dependsOnCompleted: D): State =
		{
			val remainingDependencies = state.remainingDeps.getOrElse(dependsOnCompleted, immutable.Set.empty) - work
			if(remainingDependencies.isEmpty)
			{
				val newValue = ready(state.value, dependsOnCompleted)
				new State(newValue, state.remainingDeps - dependsOnCompleted)
			}
			else
				new State(state.value, state.remainingDeps.update(dependsOnCompleted, remainingDependencies))
		}
	
		val dependsOnWork = reverseDeps.getOrElse(work, immutable.Set.empty)
		val newState = (new State(initial, remainingDeps) /: dependsOnWork)( completed )
		(newState.value, new DagInfo(newState.remainingDeps, reverseDeps - work))
	}
}
/** Constructs forward and reverse dependency map for the given Dag root node. */
private object DagInfo
{
	/** Constructs the reverse dependency map from the given Dag and 
	* puts the forward dependencies into a map */
	def apply[D <: Dag[D]](root: D): DagInfo[D] =
	{
		val remainingDeps = new mutable.HashMap[D, immutable.Set[D]]
		val reverseDeps = new mutable.HashMap[D, mutable.Set[D]]
		def visitIfUnvisited(node: D): Unit = remainingDeps.getOrElseUpdate(node, processDependencies(node))
		def processDependencies(node: D): Set[D] =
		{
			val workDependencies = node.dependencies
			workDependencies.foreach(visitIfUnvisited)
			for(dep <- workDependencies)
				reverseDeps.getOrElseUpdate(dep, new mutable.HashSet[D]) += node
			immutable.HashSet(workDependencies.toSeq: _*)
		}
		visitIfUnvisited(root)
		new DagInfo(immutable.HashMap(remainingDeps.toSeq : _*), immute(reverseDeps) )
	}
	private def immute[D](map: mutable.Map[D, mutable.Set[D]]): immutable.Map[D, immutable.Set[D]] =
	{
		val immutedSets = map.map { case (key, value) =>(key,  immutable.HashSet(value.toSeq : _*)) }
		immutable.HashMap(immutedSets.toSeq :_*)
	}
}