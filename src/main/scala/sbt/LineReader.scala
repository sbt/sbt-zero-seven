/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
package sbt

trait LineReader extends NotNull
{
	def readLine(prompt: String): Option[String]
}
class Completors(val projectAction: String, val projectNames: Iterable[String],
	val generalCommands: Iterable[String], val propertyActions: Iterable[String]) extends NotNull
class JLineReader(historyPath: Option[Path], completors: Completors, log: Logger) extends LineReader
{
	import completors._
	import jline.{ArgumentCompletor, ConsoleReader, MultiCompletor, NullCompletor, SimpleCompletor}
	
	private val generalCompletor = simpleCompletor(generalCommands)
	private val projectCompletor =
	{
		val startCompletor = simpleCompletor(projectAction :: Nil)
		val projectsCompletor = simpleCompletor(projectNames)
		val argumentCompletors = Array(startCompletor, projectsCompletor, new NullCompletor)
		new ArgumentCompletor(argumentCompletors, SingleArgumentDelimiter)
	}
	private val completor = new MultiCompletor()
	
	private val reader =
	{
		val cr = new ConsoleReader
		cr.setBellEnabled(false)
		for(historyLocation <- historyPath)
		{
			val historyFile = historyLocation.asFile
			Control.trapAndLog(log)
			{
				historyFile.getParentFile.mkdirs()
				cr.getHistory.setHistoryFile(historyFile)
			}
		}
		cr.addCompletor(completor)
		cr
	}
	
	/** Used for a single argument so that the argument can have spaces in it.*/
	object SingleArgumentDelimiter extends ArgumentCompletor.AbstractArgumentDelimiter
	{
		def isDelimiterChar(buffer: String, pos: Int) =
			(buffer.charAt(pos) == ' ') && buffer.substring(0, pos).trim.indexOf(' ') == -1
	}
	
	private def propertyCompletor(propertyNames: Iterable[String]) =
	{
		val startCompletor = simpleCompletor(propertyActions)
		val nameCompletor = simpleCompletor(propertyNames)
		val completors = Array(startCompletor, nameCompletor, new NullCompletor)
		new ArgumentCompletor(completors)
	}
	
	private def simpleCompletor(completions: Iterable[String]) = new SimpleCompletor(completions.toList.toArray)
	def setVariableCompletions(taskNames: Iterable[String], propertyNames: Iterable[String])
	{
		import scala.collection.immutable.TreeSet
		val taskCompletor = simpleCompletor(TreeSet(taskNames.toSeq : _*))
		completor.setCompletors( Array(generalCompletor, taskCompletor, projectCompletor, propertyCompletor(propertyNames)) )
	}
	def readLine(prompt: String) =
		reader.readLine(prompt) match
		{
			case null => None
			case x => Some(x)
		}
}