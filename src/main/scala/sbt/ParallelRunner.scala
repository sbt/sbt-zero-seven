/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package sbt

import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, HashSet, ListBuffer, Map, PriorityQueue, Set}

object ParallelRunner
{
	def run[D <: Dag[D]](node: D, name: D => String, action: D => Option[String], maximumTasks: Int, log: D => Logger): List[WorkFailure[D]] =
	{
		val jobScheduler = new BasicDagScheduler(node, name, (d: D) => 1)
		val distributor = new Distributor(jobScheduler, withWork(action), maximumTasks, withWork(log))
		val result = distributor.run().toList
		for( WorkFailure(work, message) <- result ) yield WorkFailure(work.data, "Error running " + name(work.data) + ": " + message)
	}
	private def withWork[D, T](f: D => T)(w: Work[D]) = f(w.data)
}
private case object Start
final class Distributor[D](scheduler: Scheduler[D], doWork: D => Option[String], workers: Int, log: D => Logger) extends NotNull
{
	require(workers > 0)
	private[this] var running = 0
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
		val Done(result, data) = complete.take()
		running -= 1
		scheduler.complete(data, result)
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
			complete put Done(result, data)
			println("Put: " + result)
		}
	}
	
	private case class Done(result: Option[String], data: D) extends NotNull
}
final case class WorkFailure[D](work: D, message: String) extends NotNull
{
	override def toString = message
}
trait Scheduler[D] extends NotNull
{
	def complete(d: D, result: Option[String]): Unit
	def hasPending: Boolean
	def next(max: Int): Seq[D]
	def result: Iterable[WorkFailure[D]]
}

private sealed abstract class Work[D](val name: String, val data: D, val cost: Int) extends Ordered[Work[D]]
{
	def compare(o: Work[D]) = pathCost compare o.pathCost
	def pathCost: Int
	override def toString = name + "(" + pathCost + ")"
}

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
private class BasicDagScheduler[D <: Dag[D]](root: D, name: D => String, cost: D => Int) extends
{
	private val ready = new PriorityQueue[Work[D]]

} with AbstractDagScheduler[D, Work[D]](root, name) {
	
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
			lazy val pathCost = reverseDeps.getOrElse(this, Nil).foldLeft(0)(_ max _.pathCost) + this.cost
		}
	protected override def setup()
	{
		super.setup()
		remainingDeps.keys.foreach(dep => dep.pathCost)
	}
}