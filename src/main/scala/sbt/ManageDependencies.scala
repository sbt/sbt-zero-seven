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
import plugins.resolver.{DependencyResolver, ChainResolver, IBiblioResolver}
import util.{Message, MessageLogger}

object ManageDependencies
{
	val DefaultIvyConfigFilename = "ivysettings.xml"
	val DefaultIvyFilename = "ivy.xml"
	val DefaultMavenFilename = "pom.xml"
	
	def defaultIvyFile(project: Path) = project / DefaultIvyFilename
	def defaultIvyConfiguration(project: Path) = project / DefaultIvyConfigFilename
	def defaultPOM(project: Path) = project / DefaultMavenFilename
	
	def update(projectDirectory: Path, outputPattern: String, managedLibDirectory: Path,
		manager: Manager, validate: Boolean, synchronize: Boolean, quiet: Boolean, addScalaTools: Boolean, log: Logger) =
	{
		val logger = new IvyLogger(log)
		Message.setDefaultLogger(logger)
		val ivy = Ivy.newInstance()
		ivy.getLoggerEngine.pushLogger(logger)
				
		def readDependencyFile(file: File, parser: ModuleDescriptorParser) =
		{
			try
			{
				Right(parser.parseDescriptor(ivy.getSettings, file.toURI.toURL, validate))
			}
			catch { case e: Exception => log.trace(e); Left("Could not read dependencies: " + e.toString) }
		}
		def readPom(pomFile: File) = readDependencyFile(pomFile, PomModuleDescriptorParser.getInstance)
		def readIvyFile(ivyFile: File) = readDependencyFile(ivyFile, XmlModuleDescriptorParser.getInstance)
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
		def addDependencies(module: ModuleID, dependencies: Iterable[ModuleID]) =
		{
			val moduleID = DefaultModuleDescriptor.newDefaultInstance(toID(module))
			for(dependency <- dependencies)
			{
				val dependencyDescriptor = new DefaultDependencyDescriptor(moduleID, toID(dependency), false, false, true)
				dependencyDescriptor.addDependencyConfiguration("*", "*")
				moduleID.addDependency(dependencyDescriptor)
			}
				
			val artifact = DefaultArtifact.newIvyArtifact(moduleID.getResolvedModuleRevisionId, moduleID.getPublicationDate)
			moduleID.setModuleArtifact(artifact)
			moduleID.check()
			Right(moduleID)
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
				else
					Left("No readable dependency configuration found.  Need " + DefaultIvyFilename + " or " + DefaultMavenFilename)
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
					{
						autodetectConfiguration()
					}
					else
					{
						log.debug("Using inline configuration.")
						configureDefaults()
						addResolvers(ivy.getSettings, resolvers, log)
					}
					if(dependencies.isEmpty && autodetectUnspecified)
						autodetectDependencies
					else
					{
						log.debug("Using inline dependencies.")
						addDependencies(module, dependencies)
					}
				}
			}
		def processModule(module: ModuleDescriptor) =
		{
			try
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
					val patternBase = managedLibDirectory.asFile.getCanonicalPath
					val pattern =
						if(patternBase.endsWith(File.separator))
							patternBase + outputPattern
						else
							patternBase + File.separatorChar + outputPattern
					ivy.retrieve(module.getModuleRevisionId, pattern, retrieveOptions)
					None
				}
			}
			catch { case e: Exception => log.trace(e); Some("Could not process dependencies: " + e.toString) }
		}
		
		this.synchronized // Ivy is not thread-safe.  In particular, it uses a static DocumentBuilder, which is not thread-safe
		{
			ivy.pushContext()
			try
			{
				moduleDescriptor.fold(Some(_), processModule)
			}
			finally { ivy.popContext() }
		}
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
				resolver.setRoot(root)
				resolver
			}
		}
	private def toID(m: ModuleID) =
	{
		import m._
		ModuleRevisionId.newInstance(organization, name, revision)
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
}
case class SimpleManager(autodetectUnspecified: Boolean, module: ModuleID, resolvers: Iterable[Resolver], dependencies: ModuleID*) extends SbtManager

final case class ModuleID(organization: String, name: String, revision: String) extends NotNull
{
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

object Configurations
{
	val Compile = "compile"
	val Test = "test"
	val Provided = "provided"
	val Javadoc = "javadoc"
	val Runtime = "runtime"
	val Sources = "sources"
	val System = "system"
	val Master = "master"
	val Default = "default"
	val Optional = "optional"
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