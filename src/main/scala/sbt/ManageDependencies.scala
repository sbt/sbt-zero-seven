/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import java.io.File

import org.apache.ivy.{core, plugins, util, Ivy}
import core.LogOptions
import core.module.descriptor.{DefaultArtifact, DefaultDependencyDescriptor, DefaultModuleDescriptor, ModuleDescriptor}
import core.module.id.ModuleRevisionId
import core.resolve.ResolveOptions
import core.retrieve.RetrieveOptions
import core.settings.IvySettings
import plugins.parser.ModuleDescriptorParser
import plugins.parser.m2.PomModuleDescriptorParser
import plugins.parser.xml.XmlModuleDescriptorParser
import plugins.repository.BasicResource
import plugins.resolver.{DependencyResolver, ChainResolver, IBiblioResolver}
import util.{Message, MessageLogger}

final case class IvyConfiguration(projectDirectory: Path, managedLibDirectory: Path, manager: Manager, validate: Boolean,
	addScalaTools: Boolean, errorIfNoConfiguration: Boolean, log: Logger)
final case class UpdateConfiguration(outputPattern: String, synchronize: Boolean, quiet: Boolean)
object ManageDependencies
{
	val DefaultIvyConfigFilename = "ivysettings.xml"
	val DefaultIvyFilename = "ivy.xml"
	val DefaultMavenFilename = "pom.xml"
	
	private def defaultIvyFile(project: Path) = project / DefaultIvyFilename
	private def defaultIvyConfiguration(project: Path) = project / DefaultIvyConfigFilename
	private def defaultPOM(project: Path) = project / DefaultMavenFilename
	
	private def withIvy(config: IvyConfiguration)(doWithIvy: (Ivy, ModuleDescriptor) => Option[String]) =
	{
		import config._
		val logger = new IvyLogger(log)
		Message.setDefaultLogger(logger)
		val ivy = Ivy.newInstance()
		ivy.getLoggerEngine.pushLogger(logger)
		
		def readDependencyFile(file: File, parser: ModuleDescriptorParser) =
			Control.trap("Could not read dependencies: ", log)
				{ Right(parser.parseDescriptor(ivy.getSettings, file.toURI.toURL, validate)) }
		
		def readPom(pomFile: File) = readDependencyFile(pomFile, PomModuleDescriptorParser.getInstance)
		def readIvyFile(ivyFile: File) = readDependencyFile(ivyFile, XmlModuleDescriptorParser.getInstance)
		def parseXMLDependencies(xml: scala.xml.NodeSeq) = parseDependencies(xml.toString)
		def parseDependencies(xml: String): Either[String, (ModuleDescriptor, CustomXmlParser.CustomParser)] =
			Control.trap("Could not read dependencies: ", log)
			{
				val parser = new CustomXmlParser.CustomParser(ivy.getSettings)
				parser.setDefaultConf("default")
				val resource = new ByteResource(xml.getBytes)
				parser.setInput(resource.openStream)
				parser.setResource(resource)
				parser.parse()
				Right((parser.getModuleDescriptor, parser))
			}
		def configure(configFile: Option[Path])
		{
			configFile match
			{
				case Some(path) => ivy.configure(path.asFile)
				case None => configureDefaults()
			}
		}
		def configureDefaults()
		{
			ivy.configureDefault
			val settings = ivy.getSettings
			if(addScalaTools)
			{
				log.debug("Added Scala Tools Releases repository.")
				addResolvers(settings, ScalaToolsReleases :: Nil, log)
			}
			settings.setBaseDir(projectDirectory.asFile)
		}
		def addDependencies(moduleID: DefaultModuleDescriptor, dependencies: Iterable[ModuleID],
			parser: Option[CustomXmlParser.CustomParser])
		{
			for(dependency <- dependencies)
			{
				val dependencyDescriptor = new DefaultDependencyDescriptor(moduleID, toID(dependency), false, false, true)
				dependency.configurations match
				{
					case None =>
					{
						parser match
						{
							case Some(p) => p.parseDepsConfs(p.getDefaultConf, dependencyDescriptor)
							case None => dependencyDescriptor.addDependencyConfiguration("default", "default")
						}
					}
					case Some(confs) =>
						parser match
						{
							case Some(p) => p.parseDepsConfs(confs, dependencyDescriptor)
							case None =>
							{
								val p = new CustomXmlParser.CustomParser(ivy.getSettings)
								p.parseDepsConfs(confs, dependencyDescriptor)
							}
						}
				}
				moduleID.addDependency(dependencyDescriptor)
			}
			
			val artifact = DefaultArtifact.newIvyArtifact(moduleID.getResolvedModuleRevisionId, moduleID.getPublicationDate)
			moduleID.setModuleArtifact(artifact)
			moduleID.check()
		}
		def autodetectConfiguration()
		{
			log.debug("Autodetecting configuration.")
			val defaultIvyConfigFile = defaultIvyConfiguration(projectDirectory).asFile
			if(defaultIvyConfigFile.canRead)
				ivy.configure(defaultIvyConfigFile)
			else
				configureDefaults()
		}
		def autodetectDependencies =
		{
			log.debug("Autodetecting dependencies.")
			val defaultPOMFile = defaultPOM(projectDirectory).asFile
			if(defaultPOMFile.canRead)
				readPom(defaultPOMFile)
			else
			{
				val defaultIvy = defaultIvyFile(projectDirectory).asFile
				if(defaultIvy.canRead)
					readIvyFile(defaultIvy)
				else if(config.errorIfNoConfiguration)
					Left("No readable dependency configuration found.  Need " + DefaultIvyFilename + " or " + DefaultMavenFilename)
				else
				{
					log.warn("No readable dependency configuration found.")
					val moduleID = DefaultModuleDescriptor.newDefaultInstance(toID(ModuleID("", "", "", None)))
					addDependencies(moduleID, Nil, None)
					Right(moduleID)
				}
			}
		}
		def moduleDescriptor =
			manager match
			{
				case MavenManager(configuration, pom) =>
				{
					log.debug("Maven configuration explicitly requested.")
					configure(configuration)
					readPom(pom.asFile)
				}
				case IvyManager(configuration, dependencies) =>
				{
					log.debug("Ivy configuration explicitly requested.")
					configure(configuration)
					readIvyFile(dependencies.asFile)
				}
				case AutoDetectManager =>
				{
					log.debug("No dependency manager explicitly specified.")
					autodetectConfiguration()
					autodetectDependencies
				}
				case sm: SbtManager =>
				{
					import sm._
					if(resolvers.isEmpty && autodetectUnspecified)
						autodetectConfiguration()
					else
					{
						log.debug("Using inline configuration.")
						configureDefaults()
						addResolvers(ivy.getSettings, resolvers, log)
					}
					if(dependencies.isEmpty && dependenciesXML.isEmpty && autodetectUnspecified)
						autodetectDependencies
					else
					{
						val moduleID = DefaultModuleDescriptor.newDefaultInstance(toID(module))
						if(dependenciesXML.isEmpty)
						{
							log.debug("Using inline dependencies specified in Scala.")
							addDependencies(moduleID, dependencies, None)
							Right(moduleID)
						}
						else
						{
							for(idAndParser <- parseXMLDependencies(wrapped(module, dependenciesXML)).right) yield
							{
								val (xmlModuleID, parser) = idAndParser
								log.debug("Using inline dependencies specified in Scala and XML.")
								xmlModuleID.getConfigurations.foreach(moduleID.addConfiguration)
								xmlModuleID.getDependencies.foreach(moduleID.addDependency)
								for(artifact <- xmlModuleID.getAllArtifacts; conf <- artifact.getConfigurations)
									moduleID.addArtifact(conf, artifact)
								addDependencies(moduleID, dependencies, Some(parser))
								moduleID
							}
						}
					}
				}
			}
		def wrapped(module: ModuleID, dependencies: scala.xml.NodeSeq) =
		{
			import module._
			<ivy-module version="2.0">
				<info organisation="{organization}" module="{name}"/>
				{dependencies}
			</ivy-module>
		}
		
		this.synchronized // Ivy is not thread-safe.  In particular, it uses a static DocumentBuilder, which is not thread-safe
		{
			ivy.pushContext()
			try
			{
				moduleDescriptor.fold(Some(_), doWithIvy(ivy, _))
			}
			finally { ivy.popContext() }
		}
	}
	def cleanCache(config: IvyConfiguration) =
	{
		def doClean(ivy: Ivy, module: ModuleDescriptor) =
			Control.trapUnit("Could not clean cache: ", config.log)
				{ ivy.getSettings.getRepositoryCacheManagers.foreach(_.clean()); None }
		
		withIvy(config)(doClean)
	}
	def update(ivyConfig: IvyConfiguration, updateConfig: UpdateConfiguration) =
	{
		def processModule(ivy: Ivy, module: ModuleDescriptor) =
		{
			import updateConfig._
			Control.trapUnit("Could not process dependencies: ", ivyConfig.log)
			{
				val resolveOptions = new ResolveOptions
				if(quiet)
					resolveOptions.setLog(LogOptions.LOG_DOWNLOAD_ONLY)
				val resolveReport = ivy.resolve(module, resolveOptions)
				if(resolveReport.hasError)
					Some(Set(resolveReport.getAllProblemMessages.toArray: _*).mkString(System.getProperty("line.separator")))
				else
				{
					val retrieveOptions = new RetrieveOptions
					retrieveOptions.setSync(synchronize)
					val patternBase = ivyConfig.managedLibDirectory.asFile.getCanonicalPath
					val pattern =
						if(patternBase.endsWith(File.separator))
							patternBase + outputPattern
						else
							patternBase + File.separatorChar + outputPattern
					ivy.retrieve(module.getModuleRevisionId, pattern, retrieveOptions)
					None
				}
			}
		}
		
		withIvy(ivyConfig)(processModule)
	}
	private def addResolvers(settings: IvySettings, resolvers: Iterable[Resolver], log: Logger)
	{
		val newDefault = new ChainResolver
		newDefault.setName("redefined-public")
		resolvers.foreach(r => newDefault.add(getResolver(r)))
		newDefault.add(settings.getDefaultResolver)
		settings.addResolver(newDefault)
		settings.setDefaultResolver(newDefault.getName)
		if(log.atLevel(Level.Debug))
		{
			log.debug("Using extra repositories:")
			resolvers.foreach(r => log.debug("\t" + r.toString))
		}
	}
	private def getResolver(r: Resolver) =
		r match
		{
			case MavenRepository(name, root) =>
			{
				val resolver = new IBiblioResolver
				resolver.setName(name)
				resolver.setM2compatible(true)
				resolver.setChangingPattern(""".*\-SNAPSHOT""")
				resolver.setRoot(root)
				resolver
			}
		}
	private def toID(m: ModuleID) =
	{
		import m._
		ModuleRevisionId.newInstance(organization, name, revision)
	}
	private class ByteResource(bytes: Array[Byte]) extends
		BasicResource("Inline XML dependencies", true, bytes.length, System.currentTimeMillis, true)
	{
		override def openStream = new java.io.ByteArrayInputStream(bytes)
	}
	
	private object CustomXmlParser extends XmlModuleDescriptorParser with NotNull
	{
		import XmlModuleDescriptorParser.Parser
		class CustomParser(settings: IvySettings) extends Parser(CustomXmlParser, settings) with NotNull
		{
			override def parseDepsConfs(confs: String, dd: DefaultDependencyDescriptor) = super.parseDepsConfs(confs, dd)
			override def getDefaultConf = super.getDefaultConf
			override def setDefaultConf(conf: String) = super.setDefaultConf(conf)
		}
	}
}

sealed abstract class Manager extends NotNull
final object AutoDetectManager extends Manager
final case class MavenManager(configuration: Option[Path], pom: Path) extends Manager
final case class IvyManager(configuration: Option[Path], dependencies: Path) extends Manager
trait SbtManager extends Manager
{
	def module: ModuleID
	def resolvers: Iterable[Resolver]
	def dependencies: Iterable[ModuleID]
	def autodetectUnspecified: Boolean
	def dependenciesXML: scala.xml.NodeSeq
}
case class SimpleManager(dependenciesXML: scala.xml.NodeSeq, autodetectUnspecified: Boolean,
	module: ModuleID, resolvers: Iterable[Resolver], dependencies: ModuleID*) extends SbtManager

final case class ModuleID(organization: String, name: String, revision: String, configurations: Option[String]) extends NotNull
{
	def this(organization: String, name: String, revision: String) = this(organization, name, revision, None)
	override def toString = organization + ":" + name + ":" + revision
}
sealed trait Resolver extends NotNull
{
	def name: String
}
sealed case class MavenRepository(name: String, root: String) extends Resolver
{
	override def toString = name + ": " + root
}
import Resolver._
object ScalaToolsReleases extends MavenRepository(ScalaToolsReleasesName, ScalaToolsReleasesRoot)
object ScalaToolsSnapshots extends MavenRepository(ScalaToolsSnapshotsName, ScalaToolsSnapshotsRoot)
object DefaultMavenRepository extends MavenRepository("Maven2 Repository", IBiblioResolver.DEFAULT_M2_ROOT)

object Resolver
{
	val ScalaToolsReleasesName = "Scala-Tools Maven2 Repository"
	val ScalaToolsSnapshotsName = "Scala-Tools Maven2 Snapshots Repository"
	val ScalaToolsReleasesRoot = "http://scala-tools.org/repo-releases"
	val ScalaToolsSnapshotsRoot = "http://scala-tools.org/repo-snapshots"
}

final class Configuration(override val toString: String) extends NotNull
object Configurations
{
	val Compile = new Configuration("compile")
	val Test = new Configuration("test")
	val IntegrationTest = new Configuration("it")
	val Provided = new Configuration("provided")
	val Javadoc = new Configuration("javadoc")
	val Runtime = new Configuration("runtime")
	val Sources = new Configuration("sources")
	val System = new Configuration("system")
	val Master = new Configuration("master")
	val Default = new Configuration("default")
	val Optional = new Configuration("optional")
}

private class IvyLogger(log: Logger) extends MessageLogger
{
	private var progressEnabled = false
	
	def log(msg: String, level: Int)
	{
		import Message._
		level match
		{
			case MSG_DEBUG | MSG_VERBOSE => log.debug(msg)
			case MSG_INFO => log.info(msg)
			case MSG_WARN => log.warn(msg)
			case MSG_ERR => log.error(msg)
		}
	}
	def rawlog(msg: String, level: Int)
	{
		log(msg, level)
	}
	def debug(msg: String) = log.debug(msg)
	def verbose(msg: String) = log.debug(msg)
	def deprecated(msg: String) = log.warn(msg)
	def info(msg: String) = log.info(msg)
	def rawinfo(msg: String) = log.info(msg)
	def warn(msg: String) = log.warn(msg)
	def error(msg: String) = log.error(msg)
	
	private def emptyList = java.util.Collections.emptyList[T forSome { type T}]
	def getProblems = emptyList
	def getWarns = emptyList
	def getErrors = emptyList

	def clearProblems = ()
	def sumupProblems = ()
	def progress = ()
	def endProgress = ()

	def endProgress(msg: String) = log.info(msg)
	def isShowProgress = false
	def setShowProgress(progress: Boolean) {}
}