/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

import scala.collection.immutable.TreeSet

object Main
{
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
		project.info("Building project " + i.name + " " + i.currentVersion.toString + " using " + project.getClass.getName)
		if(args.length == 0)
		{
			project.info("No actions specified, interactive session started.")
			interactive(project)
			printTime(project, startTime, "session")
		}
		else
		{
			((None: Option[String]) /: args)( (errorMessage, arg) => errorMessage orElse project.act(arg) ) match
			{
				case None => project.success("Build completed successfully.")
				case Some(errorMessage) => project.error("Error during build: " + errorMessage)
			}
			printTime(project, startTime, "build")
		}
	}
	
	val ShowCurrent = "current"
	val ShowActions = "actions"
	val ProjectAction = "project"
	val ShowProjectsAction = "projects"
	
	protected def interactiveCommands: Iterable[String] = basicCommands.toList ++ logLevels.toList
	private def logLevels: Iterable[String] = TreeSet.empty[String] ++ Level.elements.map(_.toString)
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
					if(isExitCommand(trimmed))
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
								currentProject.error("Invalid project name '" + projectName + "' (type 'projects' to list available projects).")
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
				Console.println("Current log level is " + project.getLevel)
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
			case Some(errorMessage) => project.error(errorMessage)
			case None => 
			{
				printTime(project, startTime, "")
				project.success("Successful.")
			}
		}
	}
	private def isExitCommand(v: String) =
	{
		val s = v.toLowerCase
		s == "exit" || s == "quit"
	}
	private def setLevel(project: Project, level: Level.Value)
	{
		project.topologicalSort.foreach(_.setLevel(level))
		Console.println("Set log level to " + project.getLevel)
	}
	private def printTime(project: Project, startTime: Long, s: String)
	{
		val endTime = System.currentTimeMillis()
		project.info("")
		val ss = if(s.isEmpty) "" else s + " "
		project.info("Total " + ss + "time: " + (endTime - startTime + 500) / 1000 + " s")
	}
}

