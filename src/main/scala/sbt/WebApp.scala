/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.File
import scala.xml.NodeSeq

object JettyRun extends ExitHook
{
	Main.registerExitHook(this)
	
	def name = "jetty-shutdown"
	def runBeforeExiting() { stop() }
	private var running: Option[Stoppable] = None
	private def started(s: Stoppable) { running = Some(s) }
	def stop()
	{
		synchronized
		{
			running.foreach(_.stop())
			running = None
		}
	}
	def apply(classpath: Iterable[Path], war: Path, defaultContextPath: String, jettyConfigurationXML: NodeSeq, jettyConfigurationFiles: Seq[File], log: Logger) =
		synchronized
		{
			def runJetty() =
			{
				val baseLoader = getClass.getClassLoader
				val loader: ClassLoader = new IntermediateLoader(classpath.map(_.asURL).toSeq.toArray, baseLoader)
				val lazyLoader = new LazyFrameworkLoader(implClassName, Array(FileUtilities.sbtJar.toURI.toURL), loader, baseLoader)
				val runner = ModuleUtilities.getObject(implClassName, lazyLoader).asInstanceOf[JettyRun]
				runner(war, defaultContextPath, jettyConfigurationXML, jettyConfigurationFiles, log)
			}
			
			if(running.isDefined)
				Some("Jetty is already running.")
			else
			{
				try
				{
					started(runJetty())
					None
				}
				catch
				{
					case e: NoClassDefFoundError => runError(e, "Jetty and its dependencies must be on the runtime classpath: ", log)
					case e => runError(e, "Error running Jetty: ", log)
				}
			}
		}
	private val implClassName = "sbt.LazyJettyRun"
	
	private def runError(e: Throwable, messageBase: String, log: Logger) =
	{
		log.trace(e)
		Some(messageBase + e.toString)
	}
}

private trait Stoppable
{
	def stop(): Unit
}
private trait JettyRun
{
	def apply(war: Path, defaultContextPath: String, jettyConfigurationXML: NodeSeq, jettyConfigurationFiles: Seq[File],
		log: Logger): Stoppable
}

/* This class starts Jetty.
* NOTE: DO NOT actively use this class.  You will see NoClassDefFoundErrors if you fail
*  to do so.Only use its name in JettyRun for reflective loading.  This allows using
*  the Jetty libraries provided on the project classpath instead of requiring them to be
*  available on sbt's classpath at startup.
*/
private object LazyJettyRun extends JettyRun
{
	import org.mortbay.jetty.Server
	import org.mortbay.jetty.nio.SelectChannelConnector
	import org.mortbay.jetty.webapp.WebAppContext
	import org.mortbay.log.Log
	import org.mortbay.xml.XmlConfiguration
	
	import java.lang.ref.{Reference, WeakReference}
	
	def apply(war: Path, defaultContextPath: String, jettyConfigurationXML: NodeSeq, jettyConfigurationFiles: Seq[File],
		log: Logger): Stoppable =
	{
		val oldLog = Log.getLog
		Log.setLog(new JettyLogger(log))
		val server = new Server
		
		if(jettyConfigurationXML.isEmpty && jettyConfigurationFiles.isEmpty)
			configureDefaults(server, war, defaultContextPath)
		else
		{
			for(x <- jettyConfigurationXML)
				(new XmlConfiguration(x.toString)).configure(server)
			for(file <- jettyConfigurationFiles)
				(new XmlConfiguration(file.toURI.toURL)).configure(server)
		}
		
		try
		{
			server.start()
			new StopServer(new WeakReference(server), oldLog)
		}
		catch { case e => server.stop(); throw e }
	}
	private def configureDefaults(server: Server, war: Path, defaultContextPath: String)
	{
		val defaultConnector = new SelectChannelConnector
		defaultConnector.setPort(8080)
		defaultConnector.setMaxIdleTime(30000)
		server.addConnector(defaultConnector)
		
		val webapp = new WebAppContext(war.asFile.getCanonicalPath, defaultContextPath)
		webapp.setClassLoader(getClass.getClassLoader)
		server.setHandler(webapp)
	}
	private class StopServer(serverReference: Reference[Server], oldLog: org.mortbay.log.Logger) extends Stoppable
	{
		def stop()
		{
			val server = serverReference.get
			if(server != null)
				server.stop()
			Log.setLog(oldLog)
		}
	}
	private class JettyLogger(delegate: Logger) extends org.mortbay.log.Logger
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
			def toString(arg: AnyRef) = if(arg == null) "" else arg.toString
			val pieces = msg.split("""\{\}""", 3)
			if(pieces.length == 1)
				pieces(0)
			else
			{
				val base = pieces(0) + toString(arg0) + pieces(1)
				if(pieces.length == 2)
					base
				else
					base + toString(arg1) + pieces(2)
			}
		}
	}
}
