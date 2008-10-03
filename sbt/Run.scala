package sbt

import scala.tools.nsc.{interpreter, util, GenericRunnerCommand, InterpreterLoop, ObjectRunner}
import util.ClassPath
import java.net.URL

object Run
{
	def console(classpath: Iterable[Path], log: Logger) =
	{
		val command = new GenericRunnerCommand(Nil, message => log.error(message))
		if(command.ok)
		{
			val settings = command.settings
			settings.classpath.value = Path.makeString(classpath)
			log.info("Starting scala interpreter...")
			log.debug("  Classpath: " + settings.classpath.value)
			log.info("")
			try
			{
				val loop = new InterpreterLoop
				loop.main(settings)
				// if JLine is used on a unix platform, we need to call restoreTerminal on
				// the underlying UnixTerminal, otherwise inputted characters won't be echoed.
				loop.in match
				{
					case reader: interpreter.JLineReader => restoreTerminal(reader)
					case _ => ()
				}
				None
			}
			catch
			{
				case e: Exception => log.trace(e); Some("Error during run: " + e.getMessage)
			}
		}
		else
			Some(command.usageMsg)
	}
	def run(mainClass: String, classpath: Iterable[Path], options: Seq[String], log: Logger) =
	{
		try
		{
			ObjectRunner.run(classpath.map(_.asFile.toURI.toURL).toList, mainClass, options.toList)
			None
		}
		catch
		{
			case e: Exception => log.trace(e); Some("Error during run: " + e.getMessage)
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
	private def restoreTerminal(reader: interpreter.JLineReader)
	{
		val terminal = reader.consoleReader.getTerminal
		try
		{
			type UnixTerminal = { def restoreTerminal: Unit }
			terminal.asInstanceOf[UnixTerminal].restoreTerminal
		}
		catch
		{
			// ignore ClassCastExceptions that will happen on Windows
			case e: Exception => ()
		}
	}
}