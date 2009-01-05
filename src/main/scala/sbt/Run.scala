/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import scala.tools.nsc.{interpreter, util, GenericRunnerCommand, InterpreterLoop, ObjectRunner, Settings}
import util.ClassPath
import java.net.URL

/** This module is an interface to starting the scala interpreter or runner.*/
object Run
{
	/** Starts an interactive scala interpreter session with the given classpath.*/
	def console(classpath: Iterable[Path], log: Logger) =
		createSettings(log)
		{
			(settings: Settings) =>
			{
				settings.classpath.value = Path.makeString(classpath)
				log.info("Starting scala interpreter...")
				log.debug("  Classpath: " + settings.classpath.value)
				log.info("")
				Control.trapUnit("Error during session: ", log)
				{
					val loop = new InterpreterLoop
					executeTrapExit(loop.main(settings), log)
				}
			}
		}
	/** Executes the given function, trapping calls to System.exit. */
	private def executeTrapExit(f: => Unit, log: Logger): Option[String] =
	{
		TrapExit(f, log) match
		{
			case Left(exitCode) =>
			{
				if(exitCode == 0)
				{
					log.debug("Exited with code 0")
					None
				}
				else
					Some("Nonzero exit code: " + exitCode)
			}
			case _ => None
		}
	}
	/** Runs the class 'mainClass' using the given classpath and options using the scala runner.*/
	def run(mainClass: String, classpath: Iterable[Path], options: Seq[String], log: Logger) =
	{
		createSettings(log)
		{
			(settings: Settings) =>
			{
				Control.trapUnit("Error during run: ", log)
				{
					val classpathURLs = classpath.map(_.asURL).toList
					val bootClasspath = FileUtilities.pathSplit(settings.bootclasspath.value)
					val extraURLs =
						for(pathString <- bootClasspath if pathString.length > 0) yield
							(new java.io.File(pathString)).toURI.toURL
					log.info("Running " + mainClass + " ...")
					log.debug("  Classpath:" + (classpathURLs ++ extraURLs).mkString("\n\t", "\n\t",""))
					executeTrapExit( ObjectRunner.run(classpathURLs ++ extraURLs, mainClass, options.toList), log )
				}
			}
		}
	}
	/** If mainClassOption is None, then the interactive scala interpreter is started with the given classpath.
	* Otherwise, the class wrapped by Some is run using the scala runner with the given classpath and
	* options. */
	def apply(mainClassOption: Option[String], classpath: Iterable[Path], options: Seq[String], log: Logger) =
	{
		mainClassOption match
		{
			case Some(mainClass) => run(mainClass, classpath, options, log)
			case None => console(classpath, log)
		}
	}
	/** Create a settings object and execute the provided function if the settings are created ok.*/
	private def createSettings(log: Logger)(f: Settings => Option[String]) =
	{
		val command = new GenericRunnerCommand(Nil, message => log.error(message))
		if(command.ok)
			f(command.settings)
		else
			Some(command.usageMsg)
	}
}