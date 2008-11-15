/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

import scala.collection.immutable.TreeSet

/** This class is the entry point for sbt.  If it is given any arguments, it interprets them
* as actions, executes the corresponding actions, and exits.  If there were no arguments provided,
* sbt enters interactive mode.*/
object Main
{
	/** The entry point for sbt.  If arguments are specified, they are interpreted as actions, executed,
	* and then the program terminates.  If no arguments are specified, the program enters interactive
	* mode.*/
	def main(args: Array[String])
	{
		val startTime = System.currentTimeMillis
		Project.loadProject match
		{
			case Left(errorMessage) => println(errorMessage)
			case Right(project) => startProject(project, args, startTime)
		}
	}
	private def startProject(project: Project, args: Array[String], startTime: Long)
	{
		val i = project.info
		project.log.info("Building project " + i.name + " " + i.currentVersion.toString + " using " + project.getClass.getName)
		if(args.length == 0)
		{
			project.log.info("No actions specified, interactive session started.")
			interactive(project)
			printTime(project, startTime, "session")
		}
		else
		{
			((None: Option[String]) /: args)( (errorMessage, arg) => errorMessage orElse project.act(arg) ) match
			{
				case None => project.log.success("Build completed successfully.")
				case Some(errorMessage) => project.log.error("Error during build: " + errorMessage)
			}
			printTime(project, startTime, "build")
		}
	}
	
	/** The name of the action that shows the current project and logging level of that project.*/
	val ShowCurrent = "current"
	/** The name of the action that shows all available actions.*/
	val ShowActions = "actions"
	/** The name of the action that sets the currently active project.*/
	val ProjectAction = "project"
	/** The name of the action that shows all available projects.*/
	val ShowProjectsAction = "projects"
	/** The list of lowercase action names that may be used to terminate the program.*/
	val TerminateActions: Iterable[String] = "exit" :: "quit" :: Nil
	
	/** The list of all available commands at the interactive prompt in addition to the tasks defined
	* by a project.*/
	protected def interactiveCommands: Iterable[String] = basicCommands.toList ++ logLevels.toList
	/** The list of logging levels.*/
	private def logLevels: Iterable[String] = TreeSet.empty[String] ++ Level.elements.map(_.toString)
	/** The list of all interactive commands other than logging level.*/
	private def basicCommands: Iterable[String] = TreeSet(ShowProjectsAction, ShowActions, ShowCurrent)
	
	def interactive(baseProject: Project)
	{
		val reader = new JLineReader(baseProject, ProjectAction, interactiveCommands)
		def loop(currentProject: Project)
		{
			reader.readLine("> ") match
			{
				case Some(line) =>
					val trimmed = line.trim
					if(trimmed.isEmpty)
						loop(currentProject)
					else if(TerminateActions.elements.contains(trimmed.toLowerCase))
						()
					else if(trimmed == ShowProjectsAction)
					{
						baseProject.topologicalSort.foreach(listProject)
						loop(currentProject)
					}
					else if(trimmed.startsWith(ProjectAction + " "))
					{
						val projectName = trimmed.substring(ProjectAction.length + 1)
						baseProject.topologicalSort.find(_.info.name == projectName) match
						{
							case Some(newProject) =>
							{
								printProject("Set current project to ", newProject)
								reader.changeProject(newProject)
								loop(newProject)
							}
							case None =>
							{
								currentProject.log.error("Invalid project name '" + projectName + "' (type 'projects' to list available projects).")
								loop(currentProject)
							}
						}
					}
					else
					{
						handleCommand(currentProject, trimmed)
						loop(currentProject)
					}
				case None => ()
			}
		}
		loop(baseProject)
	}
	private def listProject(p: Project) = printProject("\t", p)
	private def printProject(prefix: String, p: Project)
	{
		Console.println(prefix + p.info.name + " " + p.info.currentVersion)
	}
	
	private def handleCommand(project: Project, command: String)
	{
		command match
		{
			case ShowCurrent =>
			{
				printProject("Current project is ", project)
				Console.println("Current log level is " + project.log.getLevel)
			}
			case ShowActions =>
				for( (name, task) <- project.deepTasks)
					Console.println("\t" + name + task.description.map(x => ": " + x).getOrElse(""))
			case action =>
			{
				Level(action) match
				{
					case Some(level) => setLevel(project, level)
					case None => handleAction(project, action)
				}
			}
		}
	}
	private def handleAction(project: Project, action: String)
	{
		val startTime = System.currentTimeMillis
		val actionResult = project.act(action)
		actionResult match
		{
			case Some(errorMessage) => project.log.error(errorMessage)
			case None => 
			{
				printTime(project, startTime, "")
				project.log.success("Successful.")
			}
		}
	}
	private def setLevel(project: Project, level: Level.Value)
	{
		project.topologicalSort.foreach(_.log.setLevel(level))
		Console.println("Set log level to " + project.log.getLevel)
	}
	private def printTime(project: Project, startTime: Long, s: String)
	{
		val endTime = System.currentTimeMillis()
		project.log.info("")
		val ss = if(s.isEmpty) "" else s + " "
		project.log.info("Total " + ss + "time: " + (endTime - startTime + 500) / 1000 + " s")
	}
}
