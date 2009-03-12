/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
 package sbt.impl

import scala.collection.{immutable, jcl, mutable}
import scala.collection.Map
import jcl.IdentityHashMap

private[sbt] object RunTask
{
	final type Task = Project#Task
	val UnnamedName = "<anonymous>"
	def apply(root: Task, rootName: String): List[WorkFailure[Task]]  = apply(root, rootName, true)
	def apply(root: Task, rootName: String, parallelExecution: Boolean): List[WorkFailure[Task]] =
		apply(root, rootName, if(parallelExecution) Runtime.getRuntime.availableProcessors else 1)
	def apply(root: Task, rootName: String, maximumTasks: Int): List[WorkFailure[Task]]  = (new RunTask(root, rootName, maximumTasks)).run()
}
import RunTask._
private final class RunTask(root: Task, rootName: String, maximumTasks: Int) extends NotNull
{
	require(maximumTasks >= 1)
	def parallel = maximumTasks > 1
	def multiProject = allProjects.size >= 2
	def run(): List[WorkFailure[Task]]  =
	{
		withBuffered(_.startRecording())
		try { ParallelRunner.run(expandedRoot, expandedTaskName, runImpl, maximumTasks, _.manager.log) }
		finally
		{
			withBuffered(_.clearAll())
			for(project <- allProjects; saveError <- project.saveEnvironment) project.log.warn("Could not save properties for project " + project.name + ": " + saveError)
		}
	}
	private def withBuffered(f: BufferedLogger => Unit)
	{
		for(buffered <- bufferedLoggers)
			Control.trap(f(buffered))
	}
	/** Will be called in its own actor. */
	private def runImpl(action: Task): Option[String] =
	{
		val isRoot = action == expandedRoot
		val actionName = if(isRoot) rootName else expandedTaskName(action)
		val label = if(multiProject) (action.manager.name + " / " + actionName) else actionName
		def banner(event: ControlEvent.Value, firstSeparator: String, secondSeparator: String) =
			Control.trap(action.manager.log.control(event, firstSeparator + " " + label + " " + secondSeparator))
		if(isRoot)
			flush(action, true)
		if(parallel)
		{
			try { banner(ControlEvent.Start, "\n  ", "...") }
			finally { flush(action) }
		}
		banner(ControlEvent.Header, "\n==", "==")
		try { action.invoke }
		finally
		{
			banner(ControlEvent.Finish, "==", "==")
			if(parallel)
				flush(action)
		}
	}
	private def trapFinally(toTrap: => Unit)(runFinally: => Unit)
	{
		try { toTrap }
		catch { case e: Exception => () }
		finally { try { runFinally } catch { case e: Exception => () } }
	}
	private def flush(action: Task) { flush(action, false) }
	private def flush(action: Task, all: Boolean)
	{
		for(buffered <- bufferedLogger(action.manager))
			Control.trap(flush(buffered, all))
	}
	private def flush(buffered: BufferedLogger, all: Boolean)
	{
		if(all)
		{
			buffered.playAll()
			buffered.clearAll()
		}
		else
		{
			buffered.play()
			buffered.clear()
		}
	}

	/* Most of the following is for implicitly adding dependencies (see the expand method)*/
	private val projectDependencyCache = new IdentityHashMap[Project, Iterable[Project]]
	private def dependencies(project: Project) = projectDependencyCache.getOrElseUpdate(project, project.topologicalSort.dropRight(1))

	private val expandedCache = new IdentityHashMap[Task, Task]
	private def expanded(task: Task): Task = expandedCache.getOrElseUpdate(task, expandImpl(task))

	private val expandedTaskNameCache = new IdentityHashMap[Task, String]
	private def expandedTaskName(task: Task) =
		if(task == expandedRoot)
			rootName
		else
			expandedTaskNameCache.getOrElse(task, UnnamedName)

	private val nameToTaskCache = new IdentityHashMap[Project, Map[String, Task]]
	private def nameToTaskMap(project: Project): Map[String, Task] = nameToTaskCache.getOrElseUpdate(project, project.tasks)
	private def taskForName(project: Project, name: String): Option[Task] = nameToTaskMap(project).get(name)
	
	private val taskNameCache = new IdentityHashMap[Project, Map[Task, String]]
	private def taskName(task: Task) =
	{
		val project = task.manager
		taskNameCache.getOrElseUpdate(project, taskNameMap(project)).get(task)
	}
	
	private val expandedRoot = expand(root)
	private val allTasks = expandedRoot.topologicalSort
	private val allProjects = Set(allTasks.map(_.manager).toSeq : _*)
	private val bufferedLoggers = if(parallel) allProjects.toList.flatMap(bufferedLogger) else Nil
	
	/** Adds implicit dependencies, which are tasks with the same name in the project dependencies
	* of the enclosing project of the task.*/
	private def expand(root: Task): Task = expanded(root)
	private def expandImpl(task: Task): Task =
	{
		val nameOption = taskName(task)
		val explicitDependencies = task.dependencies
		val implicitDependencies = nameOption.map(name => dependencies(task.manager).flatMap(noninteractiveTask(name)) ).getOrElse(Nil)
		val allDependencies = mutable.HashSet( (explicitDependencies ++ implicitDependencies).toSeq  : _* )
		val expandedTask = task.setDependencies(allDependencies.toList.map(expanded))
		nameOption.foreach(name => expandedTaskNameCache(expandedTask) = name)
		expandedTask
	}
	private def noninteractiveTask(name: String)(project: Project): Option[Task] =
		taskForName(project, name) flatMap { task =>
			if(task.interactive)
			{
				project.log.debug("Not including task " + name + " in project " + project.name + ": interactive tasks can only be run directly.")
				None
			}
			else
				Some(task)
		}
	private def taskNameMap(project: Project) = mutable.Map(nameToTaskMap(project).map(_.swap).toSeq : _*)
	private def bufferedLogger(project: Project): Option[BufferedLogger] =
		project.log match
		{
			case buffered: BufferedLogger => Some(buffered)
			case _ => None
		}
}