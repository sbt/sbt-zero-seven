/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import scala.actors.Actor
import scala.collection.mutable.{HashMap, HashSet, ListBuffer, PriorityQueue}

object ParallelRunner
{
	def run(project: Project, action: String, maximumTasks: Int): List[JobFailure] =
	{
		def runProject(p: Project) =
		{
			p.tasks.get(action).flatMap( task =>
			{
				if(p == project || !task.interactive)
					task.run
				else
					task.runDependenciesOnly
			})
		}
		run(project, (p: Project) => p.info.name, runProject, maximumTasks)
	}
		
	// TODO: handle logging properly
	def run[D <: Dag[D]](node: D, name: D => String, action: D => Option[String], maximumTasks: Int): List[JobFailure] =
	{
		val actors = new HashMap[D, Job]
		val jobScheduler = new Scheduler(maximumTasks, node.topologicalSort.size)
		
		def make(d: D): Job = actors.getOrElseUpdate(d, createJob(d))
		def createJob(d: D): Job =
		{
			val deps = d.dependencies.map(make)
			new Job(name(d), action(d), 1, deps, jobScheduler)
		}
		val root = make(node)
		val runner =
			new Dependent
			{
				def pathCost = 0
				def act
				{
					receive
					{
						case Execute =>
						{
							jobScheduler ! Execute
							reply(receive { case DependencyComplete(r, result) => result })
							exit()
						}
					}
				}
			}
		root.register(runner)
		
		jobScheduler.start
		actors.values.foreach(_.start())
		runner.start
		
		(runner !? Execute).asInstanceOf[List[JobFailure]]
	}
}

final class Scheduler(maximumTasks: Int, jobCount: Int) extends Actor with NotNull
{
	require(maximumTasks >= 1, "Maximum tasks must be at least 1.")
	require(jobCount >= 1, "Job count must be at least 1.")
	
	def act
	{
		println("Scheduler started: maximum tasks = " + maximumTasks + ", jobs =" + jobCount)
		var runCount = 0
		var started = false
		val queued = new PriorityQueue[Job]
		val running = new HashSet[Job]
		val queuedBeforeStart = new ListBuffer[Job]
		
		def run(job: Job)
		{
			running += job
			job ! Execute
		}
		def runNext()
		{
			if(running.size < maximumTasks && !queued.isEmpty)
			{
				run(queued.dequeue())
				runNext()
			}
		}
		def checkIfComplete()
		{
			if(running.size == 0 && runCount == jobCount)
				exit()
		}
		
		while(true)
		{
			receive
			{
				case JobNotRunning =>
				{
					runCount += 1
					checkIfComplete() // won't return if done
				}
				case Schedule(job) =>
				{
					if(started)
					{
						if(running.size < maximumTasks)
							run(job)
						else
							queued += job
					}
					else
						queuedBeforeStart += job
				}
				case JobComplete(job) =>
				{
					runCount += 1
					running -= job
					checkIfComplete() // won't return if done
					runNext()
				}
				case Execute =>
				{
					if(!started)
					{
						started = true
						queued ++= queuedBeforeStart
						queuedBeforeStart.clear()
						runNext()
					}
				}
			}
		}
	}
}

final class Job(val name: String, action: => Option[String], cost: Int, dependencies: Iterable[Job], scheduler: Scheduler)
	extends Dependent with Ordered[Job] with NotNull
{
	val dependents = new HashSet[Dependent]
	private[sbt] def register(dependent: Dependent) { dependents += dependent }
	dependencies.foreach(_.register(this))
	
	lazy val pathCost: Int = cost + dependents.foldLeft(0)(_ max _.pathCost)
	
	def compare(other: Job) = pathCost - other.pathCost
	def schedule() { scheduler ! Schedule(this) }
	def act
	{
		val remainingDependencies = HashSet(dependencies.toSeq: _*)
		var result = List[JobFailure]()
		
		def completed()
		{
			val complete = DependencyComplete(this, result)
			dependents.foreach(_ ! complete)
			println("Job " + name + " complete.")
		}
		
		if(remainingDependencies.isEmpty)
			schedule()
		
		while(true)
		{
			receive
			{
				case Execute =>
				{
					result = action.map(msg => JobFailure(this, msg)).toList
					completed()
					scheduler ! JobComplete(this)
					exit()
				}
				case DependencyComplete(dependency, depResult) =>
				{
					remainingDependencies -= dependency
					result = depResult ::: result
					if(remainingDependencies.isEmpty)
					{
						if(result.isEmpty)
							schedule()
						else
						{
							completed()
							scheduler ! JobNotRunning
							exit()
						}
					}
				}
			}
		}
	}
	override def toString = "Job " + name
}
trait Dependent extends Actor with NotNull
{
	def pathCost: Int
}

final case class Schedule(job: Job) extends NotNull
final case class JobComplete(job: Job) extends NotNull
case object Execute extends NotNull
case object JobNotRunning extends NotNull
final case class DependencyComplete(dependency: Job, result: List[JobFailure]) extends NotNull
final case class JobFailure(job: Job, msg: String) extends NotNull
{
	override def toString = job.name + ": " + msg
}