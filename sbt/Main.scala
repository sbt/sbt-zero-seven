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
			case Right(project) =>
			{
				// in interactive mode, fill all undefined properties
				if(args.length > 0 || fillUndefinedProjectProperties(project.topologicalSort.toList))
					startProject(project, args, startTime)
			}
		}
	}
	private def startProject(project: Project, args: Array[String], startTime: Long)
	{
		project.log.info("Building project " + project.name + " " + project.version.toString + " using " + project.getClass.getName)
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
	/** The name of the action that sets the value of the property given as its argument.*/
	val SetAction = "set"
	/** The name of the action that gets the value of the property given as its argument.*/
	val GetAction = "get"
	
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
				{
					val trimmed = line.trim
					if(trimmed.isEmpty)
						loop(currentProject)
					else if(TerminateActions.elements.contains(trimmed.toLowerCase))
						()
					else if(trimmed.startsWith(ProjectAction + " "))
					{
						val projectName = trimmed.substring(ProjectAction.length + 1)
						baseProject.topologicalSort.find(_.name == projectName) match
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
						if(trimmed == ShowProjectsAction)
							baseProject.topologicalSort.foreach(listProject)
						else if(trimmed.startsWith(SetAction + " "))
							setProperty(currentProject, trimmed.substring(SetAction.length + 1))
						else if(trimmed.startsWith(GetAction + " "))
							getProperty(currentProject, trimmed.substring(GetAction.length + 1))
						else
							handleCommand(currentProject, trimmed)
						loop(currentProject)
					}
				}
				case None => ()
			}
		}
		
		loop(baseProject)
	}
	private def listProject(p: Project) = printProject("\t", p)
	private def printProject(prefix: String, p: Project)
	{
		Console.println(prefix + p.name + " " + p.version)
	}
	
	private def handleCommand(project: Project, command: String)
	{
		command match
		{
			case GetAction => getArgumentError(project.log)
			case SetAction => setArgumentError(project.log)
			case ShowCurrent =>
			{
				printProject("Current project is ", project)
				Console.println("Current log level is " + project.log.getLevel)
			}
			case ShowActions =>
			{
				for( (name, task) <- project.deepTasks)
					Console.println("\t" + name + task.description.map(x => ": " + x).getOrElse(""))
			}
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
		project.act(action) match
		{
			case Some(errorMessage) => project.log.error(errorMessage)
			case None =>
			{
				printTime(project, startTime, "")
				project.log.success("Successful.")
			}
		}
	}
	/** Sets the logging level on the given project.*/
	private def setLevel(project: Project, level: Level.Value)
	{
		project.topologicalSort.foreach(_.log.setLevel(level))
		Console.println("Set log level to " + project.log.getLevel)
	}
	/** Prints the elapsed time to the given project's log using the given
	* initial time and the label 's'.*/
	private def printTime(project: Project, startTime: Long, s: String)
	{
		val endTime = System.currentTimeMillis()
		project.log.info("")
		val ss = if(s.isEmpty) "" else s + " "
		project.log.info("Total " + ss + "time: " + (endTime - startTime + 500) / 1000 + " s")
	}
	/** Provides a partial message describing why the given property is undefined. */
	private def undefinedMessage(property: Project#UserProperty[_]): String =
	{
		property.resolve match
		{
			case vu: UndefinedValue => " is not defined."
			case e: ResolutionException => " has invalid value: " + e.toString
			case _ => ""
		}
	}
	/** Prompts the user for the value of undefined properties.  'first' is true if this is the first time
	* that the current property has been prompted.*/
	private def fillUndefinedProperties(project: Project, properties: List[(String, Project#Property[_])], first: Boolean): Boolean =
	{
		properties match
		{
			case (name, variable) :: tail =>
			{
				val shouldAdvanceOrQuit =
					variable match
					{
						case property: Project#UserProperty[_] =>
							if(first)
								project.log.error(" Property '" + name + "' " + undefinedMessage(property))
							val newValue = Console.readLine("  Enter new value for " + name + " : ")
							Console.println()
							if(newValue == null)
								None
							else
							{
								try
								{
									property.setStringValue(newValue)
									Some(true)
								}
								catch
								{
									case e =>
										project.log.error("Invalid value: " + e.getMessage)
										Some(false)
								}
							}
						case _ => Some(true)
					}
				shouldAdvanceOrQuit match
				{
					case Some(shouldAdvance) => fillUndefinedProperties(project, if(shouldAdvance) tail else properties, shouldAdvance)
					case None => false
				}
			}
			case Nil => true
		}
	}
	/** Iterates over the undefined properties in the given projects, prompting the user for the value of each undefined
	* property.*/
	private def fillUndefinedProjectProperties(projects: List[Project]): Boolean =
	{
		projects match
		{
			case project :: remaining =>
			{
				val uninitialized = project.uninitializedProperties.toList
				if(uninitialized.isEmpty)
					true
				else
				{
					project.log.error("Project in " + FileUtilities.printableFilename(project.info.projectDirectory) +
						" has undefined properties.")
					val result = fillUndefinedProperties(project, uninitialized, true) && fillUndefinedProjectProperties(remaining)
					project.saveEnvironment()
					result
				}
			}
			case Nil => true
		}
	}
	/** Prints the value of the property with the given name in the given project. */
	private def getProperty(project: Project, propertyName: String)
	{
		if(propertyName.isEmpty)
			project.log.error("No property name specified.")
		else
		{
			project.getPropertyNamed(propertyName) match
			{
				case Some(property) =>
				{
					property.resolve match
					{
						case u: UndefinedValue => project.log.error("Value of property '" + propertyName + "' is undefined.")
						case ResolutionException(m, e) => project.log.error(m)
						case DefinedValue(value, isInherited, isDefault) => Console.println(value.toString)
					}
				}
				case None =>
				{
					val value = System.getProperty(propertyName)
					if(value == null)
						project.log.error("No property named '" + propertyName + "' is defined.")
					else
						Console.println(value)
				}
			}
		}
	}
	/** Separates the space separated property name/value pair and stores the value in the user-defined property
	* with the given name in the given project.  If no such property exists, the value is stored in a system
	* property. */
	private def setProperty(project: Project, propertyNameAndValue: String)
	{
		val m = """(\S+)(\s+\S.*)?""".r.pattern.matcher(propertyNameAndValue)
		if(m.matches())
		{
			val name = m.group(1)
			val newValue =
			{
				val v = m.group(2)
				if(v == null) "" else v.trim
			}
			project.getPropertyNamed(name) match
			{
				case Some(property) =>
				{
					val succeeded =
						try
						{
							property.setStringValue(newValue)
							Console.println(" Set property '" + name + "' = '" + newValue + "'")
						}
						catch { case e => project.log.error("Error setting property '" + name + "' in " + project.environmentLabel + ": " + e.toString) }
					project.saveEnvironment()
				}
				case None =>
				{
					System.setProperty(name, newValue)
					project.log.info(" Set system property '" + name + "' = '" + newValue + "'")
				}
			}
		}
		else
			setArgumentError(project.log)
	}
	private def setArgumentError(log: Logger) { log.error("Invalid arguments for 'set': expected property name and new value.") }
	private def getArgumentError(log: Logger) { log.error("Invalid arguments for 'get': expected property name.") }
}
