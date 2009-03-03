/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
import sbt._
import java.io.{BufferedReader, File, InputStreamReader}

trait ScriptedTestFilter extends NotNull
{
	def accept(group: String, name: String): Boolean
}
trait Scripted extends NotNull
{
	def scriptedTests(log: Logger): Option[String]
}

object AcceptAllFilter extends ScriptedTestFilter
{
	def accept(group: String, name: String): Boolean = true
}
class ScriptedTests(testResources: Resources, filter: ScriptedTestFilter) extends Scripted
{
	def this(resourceBaseDirectory: File, filter: ScriptedTestFilter) = this(new Resources(resourceBaseDirectory), filter)
	def this(testResources: Resources) = this(testResources, AcceptAllFilter)
	def this(resourceBaseDirectory: File) = this(new Resources(resourceBaseDirectory))
	
	val ScriptFilename = "test"
	import testResources._
	
	private def includeDirectory(file: File) = file.getName != ".svn"
	def scriptedTests(log: Logger): Option[String] =
	{
		var success = true
		for(group <- baseDirectory.listFiles(DirectoryFilter) if includeDirectory(group))
		{
			log.info("Test group " + group.getName)
			for(test <- group.listFiles(DirectoryFilter) if includeDirectory(test))
			{
				val testName = test.getName
				if(!filter.accept(group.getName, testName))
					log.warn(" Test " + testName + " skipped.")
				else
					scriptedTest(test, log) match
					{
						case Some(err) =>
							log.error(" Test " + testName + " failed: " + err)
							success = false
						case None => log.info(" Test " + testName + " succeeded.")
					}
			}
		}
		if(success)
			None
		else
			Some("One or more tests failed.")
	}
	
	def scriptedTest(group: String, name: String, log: Logger): Option[String] =
		readOnlyResourceDirectory(group, name).fold(err => Some(err), testDirectory => scriptedTest(testDirectory, log))
	def scriptedTest(testDirectory: File, log: Logger): Option[String] =
	{
		(for(script <- (new TestScriptParser(testDirectory, log)).parse(new File(testDirectory, ScriptFilename)).right;
			u <- withProject(testDirectory, log)(project => script(project).toLeft(()) ).right )
		yield u).left.toOption
	}
}

/*
statement*
statement ::= ('$' | '>') word+ '[' word ']'
word ::= [^ \[\]]+
comment ::= '#' [^ \n\r]* ('\n' | '\r' | eof)
*/
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

import TestScriptParser._
private class TestScriptParser(baseDirectory: File, log: Logger) extends RegexParsers with NotNull
{
	type Statement = Project => Option[String]
	type PStatement = Statement with Positional
	
	private def evaluateList(list: List[Statement])(p: Project) = lazyFold(list){ x => x(p) }

	def script: Parser[Statement] = rep1(space ~> statement <~ space) ^^ evaluateList
	def statement: Parser[PStatement] =
		positioned
		{
			StartRegex ~! rep1(word) ~! "[" ~! word ~! "]" ^^
			{
				case start ~ command ~ open ~ result ~ close =>
					val successExpected = result.toLowerCase == SuccessLiteral.toLowerCase
					new Statement with Positional
					{
						def apply(p: Project) =
						{
							val result =
								start match
								{
									case CommandStart => evaluateCommand(command, successExpected)(p)
									case ActionStart => evaluateAction(command, successExpected)(p)
								}
							result.map(message => "{line " + pos.line + "} " + message)
						}
					}
			}
		}
	def space = ("""(\s+|(\#[^\n\r]*))""".r *)
	def word: Parser[String] =  ("\'" ~> "[^'\n\r]*".r <~ "\'")  |  ("\"" ~> "[^\"\n\r]*".r <~ "\"")  |  WordRegex
	def parse(scriptFile: File): Either[String, Project => Option[String]] =
	{
		def parseReader(reader: java.io.Reader) =
			parseAll(script, reader) match
			{
				case Success(result, next) => Right(result)
				case err: NoSuccess =>
				{
					val pos = err.next.pos
					Left("Could not parse test script '" + scriptFile.getCanonicalPath + 
					"' (" + pos.line + "," + pos.column + "): " + err.msg)
				}
			}
		FileUtilities.readValue(scriptFile, log)(parseReader)
	}
	
	private def scriptError(message: String): Some[String] = Some("Test script error: " + message)
	private def wrongArguments(commandName: String, args: List[String]): Some[String] =
		scriptError("Command '" + commandName + "' does not accept arguments (found '" + spacedString(args) + "').")
	private def wrongArguments(commandName: String, requiredArgs: String, args: List[String]): Some[String] = 
		scriptError("Wrong number of arguments to " + commandName + " command.  " + requiredArgs + " required, found: '" + spacedString(args) + "'.")
	private def evaluateCommand(command: List[String], successExpected: Boolean)(project: Project): Option[String] =
	{
		evaluate(successExpected, "Command '" + command.firstOption.getOrElse("") + "'", project)
		{
			command match
			{
				case Nil => scriptError("No command specified.")
				case "touch" :: paths => touch(paths, project)
				case "delete" :: paths => delete(paths, project)
				case "mkdir" :: paths => makeDirectories(paths, project)
				case "copy-file" :: from :: to :: Nil => copyFile(from, to, project)
				case "copy-file" :: args => wrongArguments("copy-file", "Two paths", args)
				case "sync" :: from :: to :: Nil => sync(from, to, project)
				case "sync" :: args => wrongArguments("sync", "Two directory paths", args)
				case "copy" :: paths => copy(paths, project)
				case "exists" :: paths => exists(paths, project)
				case "absent" :: paths => absent(paths, project)
				case "pause" :: Nil => readLine("Press enter to continue. "); println(); None
				case "pause" :: args => wrongArguments("pause", args)
				case "newer" :: a :: b :: Nil => newer(a, b, project)
				case "newer" :: args => wrongArguments("newer", "Two paths", args)
				case "sleep" :: time :: Nil => trap("Error while sleeping:") { Thread.sleep(time.toLong) }
				case "sleep" :: args => wrongArguments("sleep", "Time in milliseconds", args)
				case "exec" :: command :: args => execute(command, args, project)
				case "exec" :: other => wrongArguments("exec", "Command and arguments", other)
				case unknown :: arguments => scriptError("Unknown command " + unknown)
			}
		}
	}
	private def evaluate(successExpected: Boolean, label: String, project: Project)(body: => Option[String]): Option[String] =
	{
		val buffered = project.log.asInstanceOf[BufferedLogger]
		buffered.startRecording()
		val result =
			body match
			{
				case None =>
					if(successExpected) None
					else
					{
						buffered.play()
						Some(label + " succeeded (expected failure).")
					}
				case Some(failure) =>
					if(successExpected)
					{
						buffered.play()
						Some(label + " failed (expected success): " + failure)
					}
					else None
			}
		buffered.clear()
		result
	}
	private def evaluateAction(action: List[String], successExpected: Boolean)(project: Project): Option[String] =
	{
		def actionToString = action.mkString(" ")
		action match
		{
			case Nil => scriptError("No action specified.")
			case head :: Nil => evaluate(successExpected, "Action '" + actionToString + "'", project)(project.act(head))
			case x => scriptError("An action must be a single word (was '" + actionToString + "')")
		}
	}
	private def spacedString[T](l: Seq[T]) = l.mkString(" ")
	private def wrap(result: Option[String]) = result.flatMap(scriptError)
	private def trap(errorPrefix: String)(action: => Unit) = wrap( Control.trapUnit(errorPrefix, log) { action; None } )
	
	private def fromStrings(paths: List[String], project: Project) = paths.map(path => fromString(path, project))
	private def fromString(path: String, project: Project) = Path.fromString(project.info.projectPath, path)
	private def touch(paths: List[String], project: Project) =
		if(paths.isEmpty)
			scriptError("No paths specified for touch command.")
		else
			wrap(lazyFold(paths) { path => FileUtilities.touch(fromString(path, project), log) })
		
	private def delete(paths: List[String], project: Project) =
		if(paths.isEmpty)
			scriptError("No paths specified for delete command.")
		else
			wrap(FileUtilities.clean(fromStrings(paths, project), true, log))
	private def sync(from: String, to: String, project: Project) =
		wrap(FileUtilities.sync(fromString(from, project), fromString(to, project), log))
	private def copyFile(from: String, to: String, project: Project) =
		wrap(FileUtilities.copyFile(fromString(from, project), fromString(to, project), log))
	private def copy(paths: List[String], project: Project) =
		paths match
		{
			case Nil => scriptError("No paths specified for copy command.")
			case path :: Nil => scriptError("No destination specified for copy command.")
			case _ =>
				val mapped = fromStrings(paths, project).toArray
				val last = mapped.length - 1
				wrap(FileUtilities.copy(mapped.take(last), mapped(last), log).left.toOption)
		}
	private def makeDirectories(paths: List[String], project: Project) =
		fromStrings(paths, project) match
		{
			case Nil => scriptError("No paths specified for mkdir command.")
			case p => FileUtilities.createDirectories(p, project.log)
		}
	private def newer(a: String, b: String, project: Project) =
		trap("Error testing if '" + a + "' is newer than '" + b + "'")
		{
			val pathA = fromString(a, project)
			val pathB = fromString(b, project)
			pathA.exists && (!pathA.exists || pathA.lastModified > pathB.lastModified)
		}
	private def exists(paths: List[String], project: Project) =
		fromStrings(paths, project).filter(!_.exists) match
		{
			case Nil => None
			case x => Some("File(s) did not exist: " + x.mkString("[ ", " , ", " ]"))
		}
	private def absent(paths: List[String], project: Project) =
		fromStrings(paths, project).filter(_.exists) match
		{
			case Nil => None
			case x => Some("File(s) existed: " + x.mkString("[ ", " , ", " ]"))
		}
	private def execute(command: String, args: List[String], project: Project) =
	{
		if(command.trim.isEmpty)
			Some("Command was empty.")
		else
		{
			Control.trapUnit("Error running command: ", project.log)
			{
				val array = (command :: args).toArray
				val builder = (new ProcessBuilder(array: _*)).directory(project.info.projectDirectory).redirectErrorStream(true)
				val p = builder.start()
				val reader = new BufferedReader(new InputStreamReader(p.getInputStream))
				def readFully()
				{
					val line = reader.readLine()
					if(line != null)
					{
						project.log.info(line)
						readFully()
					}
				}
				readFully()
				p.waitFor()
				val exitValue = p.exitValue
				if(exitValue == 0)
					None
				else
					Some("Nonzero exit value (" + exitValue + ")")
			}
		}
	}
}
private object TestScriptParser
{
	val SuccessLiteral = "success"
	val Failure = "error"
	val CommandStart = "$"
	val ActionStart = ">"
	val WordRegex = """[^ \[\]\s'\"][^ \[\]\s]*""".r
	val StartRegex = ("[" + CommandStart + ActionStart + "]").r
	
	final def lazyFold[T](list: List[T])(f: T => Option[String]): Option[String] =
		list match
		{
			case Nil => None
			case head :: tail =>
				f(head) match
				{
					case None => lazyFold(tail)(f)
					case x => x
				}
		}
}