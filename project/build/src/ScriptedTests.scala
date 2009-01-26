/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
/*import sbt._
import java.io.File

class ScriptedTests(testResources: Resources) extends NotNull
{
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
				scriptedTest(test, log) match
				{
					case Some(err) =>
						log.error(" Test " + test.getName + " failed: " + err)
						success = false
					case None => log.info(" Test " + test.getName + " succeeded.")
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
	def space: Parser[String] = """\s*""".r
	def word: Parser[String] = WordRegex
	
	def parse(scriptFile: File): Either[String, Project => Option[String]] =
	{
		def parseReader(reader: java.io.Reader) =
			parseAll(script, reader) match
			{
				case Success(result, next) => Right(result)
				case err: NoSuccess => Left("Could not parse test script '" + scriptFile.getCanonicalPath + "': " + err.msg)
			}
		FileUtilities.readValue(scriptFile, log)(parseReader)
	}
	
	private def scriptError(message: String): Some[String] = Some("Test script error: " + message)
	private def wrongArguments(commandName: String, requiredArgs: String, args: List[String]): Some[String] = 
		scriptError("Wrong number of arguments to " + commandName + " command.  " + requiredArgs + " required, found: '" + spacedString(args) + "'.")
	private def evaluateCommand(command: List[String], successExpected: Boolean)(project: Project): Option[String] =
	{
		command match
		{
			case Nil => scriptError("No command specified.")
			case "touch" :: paths => touch(paths, project)
			case "delete" :: paths => delete(paths, project)
			case "copy-file" :: from :: to :: Nil => copyFile(from, to, project)
			case "copy-file" :: args => wrongArguments("copy-file", "Two paths", args)
			case "sync" :: from :: to :: Nil => sync(from, to, project)
			case "sync" :: args => wrongArguments("sync", "Two directory paths", args)
			case "copy" :: paths => copy(paths, project)
			case "exists" :: paths => exists(paths, project)
			case "absent" :: paths => absent(paths, project)
			case "pause" :: any => readLine("> Press enter to continue."); None
			case unknown :: arguments => scriptError("Unknown command " + unknown)
		}
	}
	private def evaluateAction(action: List[String], successExpected: Boolean)(project: Project): Option[String] =
	{
		def actionToString = action.mkString(" ")
		action match
		{
			case Nil => scriptError("No action specified.")
			case head :: Nil =>
			{
				val buffered = project.log.asInstanceOf[BufferedLogger]
				buffered.startRecording()
				val result =
					project.act(head) match
					{
						case None =>
							if(successExpected) None
							else
							{
								buffered.play()
								Some("Action '" + actionToString + "' succeeded (expected failure).")
							}
						case Some(failure) =>
							if(successExpected)
							{
								buffered.play()
								Some("Expected success, got error " + failure)
							}
							else None
					}
				buffered.clear()
				result
			}
			case x => scriptError("An action must be a single word (was '" + actionToString + "')")
		}
	}
	private def spacedString[T](l: Seq[T]) = l.mkString(" ")
	private def wrap(result: Option[String]) = result.flatMap(scriptError)
	
	private def fromStrings(paths: List[String], project: Project) = paths.map(path => fromString(path, project))
	private def fromString(path: String, project: Project) = Path.fromString(project.info.projectPath, path)
	def touch(paths: List[String], project: Project) =
		if(paths.isEmpty)
			scriptError("No paths specified for touch command.")
		else
			wrap(lazyFold(paths) { path => FileUtilities.touch(fromString(path, project), log) })
		
	def delete(paths: List[String], project: Project) =
		if(paths.isEmpty)
			scriptError("No paths specified for delete command.")
		else
			wrap(FileUtilities.clean(fromStrings(paths, project), true, log))
	def sync(from: String, to: String, project: Project) =
		wrap(FileUtilities.sync(fromString(from, project), fromString(to, project), log))
	def copyFile(from: String, to: String, project: Project) =
		wrap(FileUtilities.copyFile(fromString(from, project), fromString(to, project), log))
	def copy(paths: List[String], project: Project) =
		paths match
		{
			case Nil => scriptError("No paths specified for copy command.")
			case path :: Nil => scriptError("No destination specified for copy command.")
			case _ =>
				val mapped = fromStrings(paths, project).toArray
				val last = mapped.length - 1
				wrap(FileUtilities.copy(mapped.take(last), mapped(last), log).left.toOption)
		}
	def exists(paths: List[String], project: Project) =
		fromStrings(paths, project).filter(!_.exists) match
		{
			case Nil => None
			case x => Some("File(s) did not exist: " + x.mkString("[ ", " , ", " ]"))
		}
	def absent(paths: List[String], project: Project) =
		fromStrings(paths, project).filter(_.exists) match
		{
			case Nil => None
			case x => Some("File(s) existed: " + x.mkString("[ ", " , ", " ]"))
		}
}
private object TestScriptParser
{
	val SuccessLiteral = "success"
	val Failure = "error"
	val CommandStart = "$"
	val ActionStart = ">"
	val WordRegex = """[^ \[\]\s]+""".r
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
}*/