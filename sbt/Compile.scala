/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

abstract class CompilerCore
{
	val ClasspathOptionString = "-classpath"
	val OutputOptionString = "-d"
	
	// Returns false if there were errors, true if there were not.
	protected def process(args: List[String], log: Logger): Boolean
	def actionStartMessage(label: String): String
	def actionNothingToDoMessage: String
	def actionSuccessfulMessage: String
	def actionUnsuccessfulMessage: String

	final def apply(label: String, sources: Iterable[Path], classpathString: String, outputDirectory: Path, options: Iterable[String], log: Logger) =
	{
		log.info(actionStartMessage(label))
		val classpathOption: List[String] =
			if(classpathString.isEmpty)
				Nil
			else
				List(ClasspathOptionString, classpathString)
		val outputDir = outputDirectory.asFile
		FileUtilities.createDirectory(outputDir, log) orElse
		{
			val classpathAndOut: List[String] = OutputOptionString :: outputDir.getAbsolutePath :: classpathOption
			
			try
			{
				val sourceList = sources.map(_.asFile.getAbsolutePath).toList
				if(sourceList.isEmpty)
				{
					log.info(actionNothingToDoMessage)
					None
				}
				else
				{
					val arguments = (options ++ classpathAndOut ++ sourceList).toList
					log.debug("Arguments: " + arguments.mkString(" "))
					if(process(arguments, log))
					{
						log.info(actionSuccessfulMessage)
						None
					}
					else
						Some(actionUnsuccessfulMessage)
				}
			}
			catch
			{
				case e: Exception => log.trace(e); Some("Compiler error: " + e.getMessage)
			}
		}
	}
}
// The following code is based on scala.tools.nsc.Main and scala.tools.nsc.ScalaDoc
// Copyright 2005-2008 LAMP/EPFL
// Original author: Martin Odersky
	
object Compile extends CompilerCore
{
	protected def process(arguments: List[String], log: Logger) =
	{
		import scala.tools.nsc.{CompilerCommand, FatalError, Global, Settings, reporters, util}
		import util.FakePos
		import reporters.{ConsoleReporter, Reporter}
		var reporter: ConsoleReporter = null
		def error(msg: String) { reporter.error(FakePos("scalac"), msg) }
		def printSummary()
		{
			/*if (reporter.WARNING.count > 0) log.info(reporter.getCountString(reporter.WARNING) + " found")
			if (  reporter.ERROR.count > 0) log.info(reporter.getCountString(reporter.ERROR  ) + " found")*/
		}
		val settings = new Settings(error)
		reporter = new ConsoleReporter(settings)
		val command = new CompilerCommand(arguments, settings, error, false)
		
		object compiler extends Global(command.settings, reporter)
		if(!reporter.hasErrors)
		{
			val run = new compiler.Run
			run compile command.files
			printSummary()
		}
		!reporter.hasErrors
	}
	def actionStartMessage(label: String) = "Compiling " + label + " sources..."
	val actionNothingToDoMessage = "Nothing to compile."
	val actionSuccessfulMessage = "Compilation successful."
	def actionUnsuccessfulMessage = "Compilation unsuccessful."
}
object Scaladoc extends CompilerCore
{
	protected def process(arguments: List[String], log: Logger) =
	{
		import scala.tools.nsc.{doc, CompilerCommand, FatalError, Global, reporters, util}
		import util.FakePos
		import reporters.{ConsoleReporter, Reporter}
		var reporter: ConsoleReporter = null
		def error(msg: String) { reporter.error(FakePos("scalac"), msg) }
		def printSummary()
		{
			/*if (reporter.WARNING.count > 0) log.info(reporter.getCountString(reporter.WARNING) + " found")
			if (  reporter.ERROR.count > 0) log.info(reporter.getCountString(reporter.ERROR  ) + " found")*/
		}
		val docSettings: doc.Settings = new doc.Settings(error)
		reporter = new ConsoleReporter(docSettings)
		val command = new CompilerCommand(arguments, docSettings, error, false)
		object compiler extends Global(command.settings, reporter)
		{
			override val onlyPresentation = true
		}
		if(!reporter.hasErrors)
		{
			val run = new compiler.Run
			run compile command.files
			val generator = new doc.DefaultDocDriver
			{
				lazy val global: compiler.type = compiler
				lazy val settings = docSettings
			}
			generator.process(run.units)
			printSummary()
		}
		!reporter.hasErrors
	}
	def actionStartMessage(label: String) = "Generating API documentation for " + label + " sources..."
	val actionNothingToDoMessage = "No sources specified."
	val actionSuccessfulMessage = "API documentation generation successful."
	def actionUnsuccessfulMessage = "API documentation generation unsuccessful."
}