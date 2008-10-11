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
					loop.main(settings)
					None
				}
				catch
				{
					case e: Exception => log.trace(e); Some("Error during session: " + e.getMessage)
				}
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
					ObjectRunner.run(classpathURLs ++ extraURLs, mainClass, options.toList)
					None
				}
				catch
				{
					case e: Exception => log.trace(e); Some("Error during run: " + e.getMessage)
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