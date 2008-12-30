/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.File
import scala.xml.NodeSeq

object JettyRun
{
	def apply(war: Path, defaultContextPath: Path, jettyConfigurationXML: NodeSeq, jettyConfigurationFiles: Seq[File], log: Logger) =
	{
		import org.mortbay.jetty.Server
		import org.mortbay.jetty.nio.SelectChannelConnector
		import org.mortbay.jetty.webapp.WebAppContext
		import org.mortbay.log.Log
		import org.mortbay.xml.XmlConfiguration
		
		val oldLog = Log.getLog
		Log.setLog(new JettyLogger(log))
		
		try
		{
			val server = new Server
			def configure(configuration: XmlConfiguration) { configuration.configure(server) }
			if(jettyConfigurationXML.isEmpty && jettyConfigurationFiles.isEmpty)
			{
				val defaultConnector = new SelectChannelConnector
				defaultConnector.setPort(8080)
				defaultConnector.setMaxIdleTime(30000)
				server.addConnector(defaultConnector)
				
				val webapp = new WebAppContext(war.asFile.getCanonicalPath, defaultContextPath.asFile.getCanonicalPath)
				server.setHandler(webapp)
			}
			else
			{
				for(x <- jettyConfigurationXML)
					configure(new XmlConfiguration(x.toString))
				for(file <- jettyConfigurationFiles)
					configure(new XmlConfiguration(file.toURI.toURL))
			}
			server.start()
			server.join()
			None
		}
		catch
		{
			case e: ClassNotFoundException => log.trace(e); Some("Jetty and its dependencies must be on the classpath: " + e.toString)
			case e => log.trace(e); Some("Error running Jetty: " + e.toString)
		}
		finally
		{
			Log.setLog(oldLog)
		}
	}
}

class JettyLogger(delegate: Logger) extends org.mortbay.log.Logger
{
	def isDebugEnabled = delegate.atLevel(Level.Debug)
	def setDebugEnabled(enabled: Boolean) = delegate.setLevel(if(enabled) Level.Debug else Level.Info)

	def getLogger(name: String) = this
	def info(msg: String, arg0: AnyRef, arg1: AnyRef) { delegate.info(format(msg, arg0, arg1)) }
	def debug(msg: String, arg0: AnyRef, arg1: AnyRef) { delegate.debug(format(msg, arg0, arg1)) }
	def warn(msg: String, arg0: AnyRef, arg1: AnyRef) { delegate.warn(format(msg, arg0, arg1)) }
	def warn(msg: String, th: Throwable)
	{
		delegate.warn(msg)
		delegate.trace(th)
	}
	def debug(msg: String, th: Throwable)
	{
		delegate.debug(msg)
		delegate.trace(th)
	}
	private def format(msg: String, arg0: AnyRef, arg1: AnyRef) =
	{
		val pieces = msg.split("""\{\}""", 2)
		if(pieces.length == 1)
			pieces(0)
		else
		{
			val base = pieces(0) + arg0 + pieces(1)
			if(pieces.length == 2)
				base
			else
				base + arg1 + pieces(2)
		}
	}
}