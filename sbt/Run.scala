/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import scala.tools.nsc.{interpreter, util, GenericRunnerCommand, InterpreterLoop, ObjectRunner, Settings}
import util.ClassPath
import java.net.URL

object Run
{
	def console(classpath: Iterable[Path], log: Logger) =
		createSettings(log)
		{
			(settings: Settings) =>
			{
				settings.classpath.value = Path.makeString(classpath)
				log.info("Starting scala interpreter...")
				log.debug("  Classpath: " + settings.classpath.value)
				log.info("")
				try
				{
					val loop = new InterpreterLoop
					executeTrapExit(loop.main(settings), log)
				}
				catch
				{
					case e: Exception => log.trace(e); Some("Error during session: " + e.toString)
				}
			}
		}
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
	def run(mainClass: String, classpath: Iterable[Path], options: Seq[String], log: Logger) =
	{
		createSettings(log)
		{
			(settings: Settings) =>
			{
				try
				{
					val classpathURLs = classpath.map(_.asURL).toList
					val bootClasspath = FileUtilities.pathSplit(settings.bootclasspath.value)
					val extraURLs =
						for(pathString <- bootClasspath if pathString.length > 0) yield
							(new java.io.File(pathString)).toURI.toURL
					log.info("Running " + mainClass + " ...")
					executeTrapExit( ObjectRunner.run(classpathURLs ++ extraURLs, mainClass, options.toList), log )
				}
				catch
				{
					case e: Exception => log.trace(e); Some("Error during run: " + e.toString)
				}
			}
		}
	}
	def apply(mainClassOption: Option[String], classpath: Iterable[Path], options: Seq[String], log: Logger) =
	{
		mainClassOption match
		{
			case Some(mainClass) => run(mainClass, classpath, options, log)
			case None => console(classpath, log)
		}
	}
	private def createSettings(log: Logger)(f: Settings => Option[String]) =
	{
		val command = new GenericRunnerCommand(Nil, message => log.error(message))
		if(command.ok)
			f(command.settings)
		else
			Some(command.usageMsg)
	}
}