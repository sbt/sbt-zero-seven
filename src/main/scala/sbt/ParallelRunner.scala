/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package sbt

import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, HashSet, ListBuffer, Map, PriorityQueue, Set}

/** Interface to the Distributor/Scheduler system for running tasks with dependencies described by a directed acyclic graph.*/
object ParallelRunner
{
	/** Executes work for nodes in an acyclic directed graph with root node `node`.  The name of a node is provided
	* by the `name` function, the work to perform for a node by `action`, and the logger to use for a node by `log`.
	* The maximum number of tasks to execute simultaneously is `maximumTasks`. */
	def run[D <: Dag[D]](node: D, name: D => String, action: D => Option[String], maximumTasks: Int, log: D => Logger): List[WorkFailure[D]] =
	{
		 // Create a scheduler that gives each node a uniform cost.  This could be modified to include more information about
		 // a node, such as the size of input files
		val jobScheduler = new BasicDagScheduler(node, name, (d: D) => 1)
		val distributor = new Distributor(jobScheduler, withWork(action), maximumTasks, withWork(log))
		val result = distributor.run().toList
		for( WorkFailure(work, message) <- result ) yield WorkFailure(work.data, "Error running " + name(work.data) + ": " + message)
	}
	private def withWork[D, T](f: D => T)(w: Work[D]) = f(w.data)
}
final class Distributor[D](scheduler: Scheduler[D], doWork: D => Option[String], workers: Int, log: D => Logger) extends NotNull
{
	require(workers > 0)
	/** The number of threads currently running. */
	private[this] var running = 0
	/** Pending notifications of completed work. */
	private[this] val complete = new java.util.concurrent.LinkedBlockingQueue[Done]
	
	def run() =
	{
		runNext()
		scheduler.result
	}
	private def runNext()
	{
		val done = next()
		if(!done)
		{
			completeNext()
			runNext()
		}
	}
	private def atMaximum = running >= workers
	private def availableWorkers = Math.max(workers - running, 0)
	private def isIdle = running == 0
	private def next() =
	{
		if(atMaximum)
			false
		else if(scheduler.hasPending)
		{
			val available = availableWorkers
			val next = scheduler.next(available)
			val nextSize = next.size
			if(nextSize <= 0)
				assume(!isIdle)
			else
			{
				assume(nextSize > 0)
				assume(nextSize <= available)
				next.foreach(process)
			}
			false
		}
		else
			isIdle
	}
	private def completeNext()
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
	def complete(d: D, result: Option[String]): Unit
	/** Returns true if there is any more work to be done, although remaining work can be blocked
	* waiting for currently running work to complete.*/
	def hasPending: Boolean
	/** Returns up to 'max' units of work.  `max` is always positive.  The returned sequence cannot be empty if there is
	* no work currently be processed.*/
	def next(max: Int): Seq[D]
	/** A list of failures that occurred, as reported by the `complete` method. */
	def result: Iterable[WorkFailure[D]]
}

/** Represents a named unit of work with some cost associated with performing the work itself.*/
private sealed abstract class Work[D](val name: String, val data: D, val cost: Int) extends Ordered[Work[D]]
{
	/** Compares two work units by their pathCost.*/
	final def compare(o: Work[D]) = pathCost compare o.pathCost
	/** A full cost of the work that includes indirect costs, such as costs of dependencies.*/
	def pathCost: Int
	override final def toString = name + "(" + pathCost + ")"
}
/** A scheduler for nodes of a directed-acyclic graph with root node `root`.*/
private[sbt] abstract class AbstractDagScheduler[D <: Dag[D], W](root: D, name: D => String) extends Scheduler[W]
{
	protected val remainingDeps: Map[W, Set[W]] = new HashMap
	protected val reverseDeps: Map[W, Set[W]] = new HashMap
	protected val errors = new ListBuffer[WorkFailure[W]]
	
	setup()
	
	def result = errors.readOnly
	def complete(work: W, result: Option[String])
	{
		result match
		{
			case None => workComplete(work)
			case Some(errorMessage) =>
			{
				def clear(w: W)
				{
					remainingDeps -= w
					for(deps <- reverseDeps.removeKey(w); dep <- deps)
						clear(dep)
				}
				clear(work)
				errors += WorkFailure(work, errorMessage)
			}
		}
	}
	private def workComplete(work: W)
	{
		for(deps <- reverseDeps.removeKey(work); dep <- deps; depRemaining <- remainingDeps.get(dep))
		{
			depRemaining -= work
			if(depRemaining.isEmpty)
			{
				remainingDeps -= dep
				workReady(dep)
			}
		}
	}
	protected def workReady(work: W): Unit
	def hasReady: Boolean
	def hasPending = hasReady || !remainingDeps.isEmpty
	protected def workConstructor(name: String, data: D): W
	
	protected def setup()
	{
		setup(root)
		val startReady = for( (key, value) <- remainingDeps if(value.isEmpty)) yield key
		remainingDeps --= startReady
		startReady.foreach(workReady)
	}
	private def setup(node: D)
	{
		val dToWork = new HashMap[D, W]()
		def getWork(node: D) = dToWork.getOrElseUpdate(node, createWork(node))
		def createWork(node: D): W =
		{
			val workDependencies = node.dependencies.map(getWork(_))
			val w = workConstructor(name(node), node)
			remainingDeps(w) = HashSet(workDependencies.toSeq: _*)
			for(dep <- workDependencies)
				reverseDeps.getOrElseUpdate(dep, new HashSet[W]) += w
			w
		}
		getWork(node)
	}
}
/** A scheduler for nodes of a directed-acyclic graph.  It requires the root of the graph, a function to obtain the
* name of the node, and a function to obtain the cost of the node. It prioritizes work by its pathCost.*/
private class BasicDagScheduler[D <: Dag[D]](root: D, name: D => String, cost: D => Int) extends AbstractDagScheduler[D, Work[D]](root, name)
{
	private[this] lazy val ready = new PriorityQueue[Work[D]]
	def next(max: Int): List[Work[D]] =
	{
		require(max > 0)
		def nextImpl(remaining: Int): List[Work[D]] =
			if(remaining <= 0 || ready.isEmpty)
				Nil
			else
				ready.dequeue :: nextImpl(remaining - 1)
		nextImpl(max)
	}
	
	protected def workReady(dep: Work[D]) { ready += dep }
	def hasReady = !ready.isEmpty
	protected def workConstructor(name: String, data: D) =
		new Work(name, data, cost(data)) {
			// compute the cost of the longest dependency path required to execute this work
			lazy val pathCost = reverseDeps.getOrElse(this, Nil).foldLeft(0)(_ max _.pathCost) + this.cost
		}
	protected override def setup()
	{
		// let the parent populate the dependency information
		super.setup()
		// force computation of the pathCost because it uses `reverseDeps`, which will change after starting
		remainingDeps.keys.foreach(dep => dep.pathCost)
	}
}