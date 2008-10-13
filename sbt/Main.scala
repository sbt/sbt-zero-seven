/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

object Main
{
	def main(args: Array[String])
	{
		val startTime = System.currentTimeMillis
		for(project <- Project.loadProject.right)
		{
			val i = project.info
			project.info("Building project " + i.name + " " + i.currentVersion.toString + " using " + project.getClass.getName)
			if(args.length == 0)
			{
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
	}
	
	val ShowLevel = "level"
	val ShowActions = "actions"
	protected def interactiveCommands: Iterable[String] =
		ShowActions :: ShowLevel :: Level.elements.map(_.toString).toList
		
	def interactive(project: Project)
	{
		project.info("No actions specified, interactive session started.")
		
		val reader = new JLineReader(project.taskNames ++ interactiveCommands)
		def loop()
		{
			reader.readLine("> ") match
			{
				case Some(line) =>
					val trimmed = line.trim
					if(isExitCommand(trimmed))
						()
					else
					{
						handleCommand(project, trimmed)
						loop()
					}
				case None => ()
			}
		}
		loop()
	}
	
	private def handleCommand(project: Project, command: String)
	{
		command match
		{
			case ShowLevel => Console.println("Current log level is " + project.getLevel)
			case ShowActions => 
				for( (name, task) <- project.tasks)
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
		project.info("Total " + ss + "time: " + (endTime - startTime) / 1000 + " s")
	}
}
