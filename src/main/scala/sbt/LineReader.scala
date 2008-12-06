/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

trait LineReader extends NotNull
{
	def readLine(prompt: String): Option[String]
}
class JLineReader(initialProject: Project, projectAction: String, generalCommands: Iterable[String]) extends LineReader
{
	import jline.{ArgumentCompletor, ConsoleReader, MultiCompletor, NullCompletor, SimpleCompletor}
	
	private val generalCompletor = simpleCompletor(generalCommands)
	private val projectCompletor =
	{
		val startCompletor = simpleCompletor(projectAction :: Nil)
		val projectsCompletor = simpleCompletor(initialProject.topologicalSort.map(_.name))
		val argumentCompletors = Array(startCompletor, projectsCompletor, new NullCompletor)
		new ArgumentCompletor(argumentCompletors, SingleArgumentDelimiter)
	}
	private val completor = new MultiCompletor()
	changeProject(initialProject)
	
	private val reader =
	{
		val cr = new ConsoleReader
		cr.addCompletor(completor)
		cr
	}
	
	/** Used for a single argument so that the argument can have spaces in it.*/
	object SingleArgumentDelimiter extends ArgumentCompletor.AbstractArgumentDelimiter
	{
		def isDelimiterChar(buffer: String, pos: Int) =
			(buffer.charAt(pos) == ' ') && buffer.substring(0, pos).trim.indexOf(' ') == -1
	}
	
	private def propertyCompletor(project: Project) =
	{
		val startCompletor = simpleCompletor(Main.GetAction :: Main.SetAction :: Nil)
		val nameCompletor = simpleCompletor(project.propertyNames)
		val completors = Array(startCompletor, nameCompletor, new NullCompletor)
		new ArgumentCompletor(completors)
	}
	
	private def simpleCompletor(completions: Iterable[String]) = new SimpleCompletor(completions.toList.toArray)
	def changeProject(project: Project)
	{
		val taskCompletor = simpleCompletor(project.taskNames)
		completor.setCompletors( Array(generalCompletor, taskCompletor, projectCompletor, propertyCompletor(project)) )
	}
	def readLine(prompt: String) =
		reader.readLine(prompt) match
		{
			case null => None
			case x => Some(x)
		}
}