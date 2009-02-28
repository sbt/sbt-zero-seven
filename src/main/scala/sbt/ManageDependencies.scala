/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import java.io.File

import org.apache.ivy.{core, plugins, util, Ivy}
import core.LogOptions
import core.module.descriptor.{DefaultArtifact, DefaultDependencyDescriptor, DefaultModuleDescriptor, MDArtifact, ModuleDescriptor}
import core.module.id.ModuleRevisionId
import core.resolve.ResolveOptions
import core.retrieve.RetrieveOptions
import core.settings.IvySettings
import plugins.matcher.PatternMatcher
import plugins.parser.ModuleDescriptorParser
import plugins.parser.m2.{PomModuleDescriptorParser,PomModuleDescriptorWriter}
import plugins.parser.xml.XmlModuleDescriptorParser
import plugins.repository.{BasicResource, Resource}
import plugins.resolver.{DependencyResolver, ChainResolver, IBiblioResolver}
import util.{Message, MessageLogger}

final class IvyPaths(val projectDirectory: Path, val managedLibDirectory: Path, val cacheDirectory: Option[Path]) extends NotNull
final class IvyFlags(val validate: Boolean, val addScalaTools: Boolean, val errorIfNoConfiguration: Boolean) extends NotNull
final class IvyConfiguration(val paths: IvyPaths, val manager: Manager, val flags: IvyFlags, val log: Logger) extends NotNull
final class UpdateConfiguration(val outputPattern: String, val synchronize: Boolean, val quiet: Boolean) extends NotNull
object ManageDependencies
{
	val DefaultIvyConfigFilename = "ivysettings.xml"
	val DefaultIvyFilename = "ivy.xml"
	val DefaultMavenFilename = "pom.xml"
	
	private def defaultIvyFile(project: Path) = project / DefaultIvyFilename
	private def defaultIvyConfiguration(project: Path) = project / DefaultIvyConfigFilename
	private def defaultPOM(project: Path) = project / DefaultMavenFilename
	
	/** Configures Ivy using the provided configuration 'config' and calls 'doWithIvy'.  This method takes care of setting up and cleaning up Ivy.*/
	private def withIvy(config: IvyConfiguration)(doWithIvy: (Ivy, ModuleDescriptor) => Option[String]) =
		withIvyValue(config)( (ivy, module) => doWithIvy(ivy, module).toLeft(()) ).left.toOption
	/** Configures Ivy using the provided configuration 'config' and calls 'doWithIvy'.  This method takes care of setting up and cleaning up Ivy.*/
	private def withIvyValue[T](config: IvyConfiguration)(doWithIvy: (Ivy, ModuleDescriptor) => Either[String, T]) =
	{
		import config._
		val logger = new IvyLogger(log)
		Message.setDefaultLogger(logger)
		val ivy = Ivy.newInstance()
		ivy.getLoggerEngine.pushLogger(logger)
		
		/** Parses the given dependency management file using the provided parser.*/
		def readDependencyFile(file: File, parser: ModuleDescriptorParser) =
			Control.trap("Could not read dependencies: ", log)
				{ Right(parser.parseDescriptor(ivy.getSettings, file.toURI.toURL, flags.validate)) }
		
		/** Parses the given Maven pom 'pomFile'.*/
		def readPom(pomFile: File) = readDependencyFile(pomFile, PomModuleDescriptorParser.getInstance)
		/** Parses the given Ivy file 'ivyFile'.*/
		def readIvyFile(ivyFile: File) = readDependencyFile(ivyFile, XmlModuleDescriptorParser.getInstance)
		/** Parses the given in-memory Ivy file 'xml', using the existing 'moduleID' and specifying the given 'defaultConfiguration'. */
		def parseXMLDependencies(xml: scala.xml.NodeSeq, moduleID: DefaultModuleDescriptor, defaultConfiguration: String) =
			parseDependencies(xml.toString, moduleID, defaultConfiguration)
		/** Parses the given in-memory Ivy file 'xml', using the existing 'moduleID' and specifying the given 'defaultConfiguration'. */
		def parseDependencies(xml: String, moduleID: DefaultModuleDescriptor, defaultConfiguration: String): Either[String, CustomXmlParser.CustomParser] =
			Control.trap("Could not read dependencies: ", log)
			{
				val parser = new CustomXmlParser.CustomParser(ivy.getSettings)
				parser.setMd(moduleID)
				parser.setDefaultConf(defaultConfiguration)
				val resource = new ByteResource(xml.getBytes)
				parser.setInput(resource.openStream)
				parser.setResource(resource)
				parser.parse()
				Right(parser)
			}
		/** Configures Ivy using the specified Ivy configuration file.  This method is used when the manager is explicitly requested to be MavenManager or
		* IvyManager.  If a file is not specified, Ivy is configured with defaults and scala-tools releases is added as a repository.*/
		def configure(configFile: Option[Path])
		{
			configFile match
			{
				case Some(path) => ivy.configure(path.asFile)
				case None =>
					configureDefaults()
					scalaTools()
			}
		}
		/** Adds the scala-tools.org releases maven repository to the list of resolvers if configured to do so in IvyFlags.*/
		def scalaTools()
		{
			if(flags.addScalaTools)
			{
				log.debug("Added Scala Tools Releases repository.")
				addResolvers(ivy.getSettings, ScalaToolsReleases :: Nil, log)
			}
		}
		/** Configures Ivy using defaults.  This is done when no ivy-settings.xml exists and no inline configurations or resolvers are specified. */
		def configureDefaults()
		{
			ivy.configureDefault
			val settings = ivy.getSettings
			for(dir <- paths.cacheDirectory) settings.setDefaultCache(dir.asFile)
			settings.setBaseDir(paths.projectDirectory.asFile)
		}
		/** This method is used to add inline dependencies to the provided module.  It properly handles the
		* different ways of specifying configurations and dependencies. */
		def addDependencies(moduleID: DefaultModuleDescriptor, dependencies: Iterable[ModuleID],
			parser: CustomXmlParser.CustomParser, defaultConfiguration: Option[Configuration])
		{
			for(dependency <- dependencies)
			{
				val dependencyDescriptor = new DefaultDependencyDescriptor(moduleID, toID(dependency), false, false, true)
				dependency.configurations match
				{
					case None => // The configuration for this dependency was not explicitly specified, so use the default
						parser.parseDepsConfs(parser.getDefaultConf, dependencyDescriptor)
					case Some(confs) => // The configuration mapping (looks like: test->default) was specified for this dependency
						parser.parseDepsConfs(confs, dependencyDescriptor)
				}
				moduleID.addDependency(dependencyDescriptor)
			}
			addMainArtifact(moduleID)
		}
		def addMainArtifact(moduleID: DefaultModuleDescriptor)
		{
			val artifact = DefaultArtifact.newIvyArtifact(moduleID.getResolvedModuleRevisionId, moduleID.getPublicationDate)
			moduleID.setModuleArtifact(artifact)
			moduleID.check()
		}
		/** Called to configure Ivy when the configured dependency manager is SbtManager and inline configuration is specified or if the manager
		* is AutodetectManager.  It will configure Ivy with an 'ivy-settings.xml' file if there is one, or configure the defaults and add scala-tools as
		* a repository otherwise.*/
		def autodetectConfiguration()
		{
			log.debug("Autodetecting configuration.")
			val defaultIvyConfigFile = defaultIvyConfiguration(paths.projectDirectory).asFile
			if(defaultIvyConfigFile.canRead)
				ivy.configure(defaultIvyConfigFile)
			else
			{
				configureDefaults()
				scalaTools()
			}
		}
		/** Called to determine dependencies when the dependency manager is SbtManager and no inline dependencies (Scala or XML) are defined
		* or if the manager is AutodetectManager.  It will try to read from pom.xml first and then ivy.xml if pom.xml is not found.  If neither is found,
		* Ivy is configured with defaults unless IvyFlags.errorIfNoConfiguration is true, in which case an error is generated.*/
		def autodetectDependencies(module: ModuleRevisionId) =
		{
			log.debug("Autodetecting dependencies.")
			val defaultPOMFile = defaultPOM(paths.projectDirectory).asFile
			if(defaultPOMFile.canRead)
				readPom(defaultPOMFile)
			else
			{
				val defaultIvy = defaultIvyFile(paths.projectDirectory).asFile
				if(defaultIvy.canRead)
					readIvyFile(defaultIvy)
				else if(flags.errorIfNoConfiguration)
					Left("No readable dependency configuration found.  Need " + DefaultIvyFilename + " or " + DefaultMavenFilename)
				else
				{
					log.warn("No readable dependency configuration found, using defaults.")
					val moduleID = DefaultModuleDescriptor.newDefaultInstance(module)
					addMainArtifact(moduleID)
					Right(moduleID)
				}
			}
		}
		/** Creates an Ivy module descriptor according the manager configured.*/
		def moduleDescriptor =
			config.manager match
			{
				case mm: MavenManager =>
				{
					log.debug("Maven configuration explicitly requested.")
					configure(mm.configuration)
					readPom(mm.pom.asFile)
				}
				case im: IvyManager =>
				{
					log.debug("Ivy configuration explicitly requested.")
					configure(im.configuration)
					readIvyFile(im.dependencies.asFile)
				}
				case adm: AutoDetectManager =>
				{
					log.debug("No dependency manager explicitly specified.")
					autodetectConfiguration()
					autodetectDependencies(toID(adm.module))
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
						val extra = if(flags.addScalaTools) ScalaToolsReleases :: resolvers.toList else resolvers
						addResolvers(ivy.getSettings, extra, log)
					}
					if(dependencies.isEmpty && dependenciesXML.isEmpty && autodetectUnspecified)
						autodetectDependencies(toID(module))
					else
					{
						val moduleID =
							defaultConfiguration match
							{
								case Some(defaultConf) => // inline configurations used
								{
									val mod = new DefaultModuleDescriptor(toID(module), "release", null, false)
									mod.setLastModified(System.currentTimeMillis)
									configurations.foreach(config => mod.addConfiguration(config.toIvyConfiguration))
									mod.addArtifact(defaultConf.name, new MDArtifact(mod, module.name, "jar", "jar"))
									mod
								}
								// no inline configurations, so leave Ivy to its defaults
								case None => DefaultModuleDescriptor.newDefaultInstance(toID(module))
							}
						log.debug("Using inline dependencies specified in Scala" + (if(dependenciesXML.isEmpty) "." else " and XML."))
						val defaultConf = defaultConfiguration match { case None => "default"; case Some(dc) => dc.name }
						for(parser <- parseXMLDependencies(wrapped(module, dependenciesXML), moduleID, defaultConf).right) yield
						{
							addDependencies(moduleID, dependencies, parser, defaultConfiguration)
							moduleID
						}
					}
				}
			}
		/** Creates a full ivy file for 'module' using the 'dependencies' XML as the part after the %lt;info&gt;...%lt;/info&gt; section. */
		def wrapped(module: ModuleID, dependencies: scala.xml.NodeSeq) =
		{
			import module._
			<ivy-module version="2.0">
				<info organisation={organization} module={name} revision={revision}/>
				{dependencies}
			</ivy-module>
		}
		
		this.synchronized // Ivy is not thread-safe.  In particular, it uses a static DocumentBuilder, which is not thread-safe
		{
			ivy.pushContext()
			try
			{
				moduleDescriptor.right.flatMap(doWithIvy(ivy, _))
			}
			finally { ivy.popContext() }
		}
	}
	/** Clears the Ivy cache, as configured by 'config'. */
	def cleanCache(config: IvyConfiguration) =
	{
		def doClean(ivy: Ivy, module: ModuleDescriptor) =
			Control.trapUnit("Could not clean cache: ", config.log)
				{ ivy.getSettings.getRepositoryCacheManagers.foreach(_.clean()); None }
		
		withIvy(config)(doClean)
	}
	/** Creates a Maven pom from the given Ivy configuration*/
	def makePom(config: IvyConfiguration, output: File) =
	{
		def doMakePom(ivy: Ivy, module: ModuleDescriptor) =
			Control.trapUnit("Could not make pom: ", config.log)
			{
				PomModuleDescriptorWriter.write(module, DefaultConfigurationMapping, output)
				None
			}
		withIvy(config)(doMakePom)
	}
	/** Resolves and retrieves dependencies.  'ivyConfig' is used to produce an Ivy file and configuration.
	* 'updateConfig' configures the actual resolution and retrieval process. */
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
					val patternBase = ivyConfig.paths.managedLibDirectory.asFile.getCanonicalPath
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
	/** Sets the resolvers for 'settings' to 'resolvers'.  This is done by creating a new chain and making it the default. */
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
	/** Converts the given sbt resolver into an Ivy resolver..*/
	private def getResolver(r: Resolver) =
		r match
		{
			case repo: MavenRepository =>
			{
				val resolver = new IBiblioResolver
				resolver.setName(repo.name)
				resolver.setM2compatible(true)
				resolver.setChangingPattern(""".*\-SNAPSHOT""")
				resolver.setRoot(repo.root)
				resolver
			}
		}
	/** Converts the given sbt module id into an Ivy ModuleRevisionId.*/
	private def toID(m: ModuleID) =
	{
		import m._
		ModuleRevisionId.newInstance(organization, name, revision)
	}
	/** An implementation of Ivy's Resource class that provides the Ivy file from a byte array.  This is used to support
	* inline Ivy file XML.*/
	private class ByteResource(bytes: Array[Byte]) extends
		BasicResource("Inline XML dependencies", true, bytes.length, System.currentTimeMillis, true)
	{
		override def openStream = new java.io.ByteArrayInputStream(bytes)
	}
	/** Subclasses the default Ivy file parser in order to provide access to protected methods.*/
	private object CustomXmlParser extends XmlModuleDescriptorParser with NotNull
	{
		import XmlModuleDescriptorParser.Parser
		class CustomParser(settings: IvySettings) extends Parser(CustomXmlParser, settings) with NotNull
		{
			/** Overridden because the super implementation overwrites the module descriptor.*/
			override def setResource(res: Resource) {}
			override def setMd(md: DefaultModuleDescriptor) = super.setMd(md)
			override def parseDepsConfs(confs: String, dd: DefaultDependencyDescriptor) = super.parseDepsConfs(confs, dd)
			override def getDefaultConf = super.getDefaultConf
			override def setDefaultConf(conf: String) = super.setDefaultConf(conf)
		}
	}
}

private object DefaultConfigurationMapping extends PomModuleDescriptorWriter.ConfigurationScopeMapping(new java.util.HashMap)
{
	override def getScope(confs: Array[String]) =
	{
		Configurations.defaultMavenConfigurations.find(conf => confs.contains(conf.name)) match
		{
			case Some(conf) => conf.name
			case None =>
				if(confs.isEmpty || confs(0) == Configurations.Default.name)
					null
				else
					confs(0)
		}
	}
	override def isOptional(confs: Array[String]) = confs.isEmpty || (confs.length == 1 && confs(0) == Configurations.Optional.name)
}

sealed abstract class Manager extends NotNull
/** This explicitly requests auto detection as a dependency manager.  It will first check for a 'pom.xml' file and if that does not exist, an 'ivy.xml' file.
* Ivy is configured using the detected file or uses defaults.*/
final class AutoDetectManager(val module: ModuleID) extends Manager
/** This explicitly requests that the Maven pom 'pom' be used to determine dependencies.  An Ivy configuration file to use may be specified in
* 'configuration', since Ivy currently cannot extract Maven repositories from a pom file. Otherwise, defaults are used.*/
final class MavenManager(val configuration: Option[Path], val pom: Path) extends Manager
/** This explicitly requests that the Ivy file 'dependencies' be used to determine dependencies.  An Ivy configuration file to use may be specified in
* 'configuration'.  Otherwise, defaults are used.*/
final class IvyManager(val configuration: Option[Path], val dependencies: Path) extends Manager
/** This manager directly specifies the dependencies, resolvers, and configurations through sbt wrapper classes and through an in-memory
* Ivy XML file. */
sealed trait SbtManager extends Manager
{
	def module: ModuleID
	def resolvers: Iterable[Resolver]
	def dependencies: Iterable[ModuleID]
	def autodetectUnspecified: Boolean
	def dependenciesXML: scala.xml.NodeSeq
	def configurations: Iterable[Configuration]
	def defaultConfiguration: Option[Configuration]
}
final class SimpleManager private[sbt] (val dependenciesXML: scala.xml.NodeSeq, val autodetectUnspecified: Boolean,
	val module: ModuleID, val resolvers: Iterable[Resolver], val configurations: Iterable[Configuration],
	val defaultConfiguration: Option[Configuration], val dependencies: ModuleID*) extends SbtManager

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
/** Represents an Ivy configuration. */
final class Configuration(val name: String, val description: String, val isPublic: Boolean, val extendsConfigs: List[Configuration], val transitive: Boolean) extends NotNull {
	require(name != null && !name.isEmpty)
	require(description != null)
	def this(name: String) = this(name, "", true, Nil, true)
	def describedAs(newDescription: String) = new Configuration(name, newDescription, isPublic, extendsConfigs, transitive)
	def extend(configs: Configuration*) = new Configuration(name, description, isPublic, configs.toList ::: extendsConfigs, transitive)
	def notTransitive = new Configuration(name, description, isPublic, extendsConfigs, false)
	def hide = new Configuration(name, description, false, extendsConfigs, transitive)
	override def toString = name
	import org.apache.ivy.core.module.descriptor.{Configuration => IvyConfig}
	import IvyConfig.Visibility._
	def toIvyConfiguration = new IvyConfig(name, if(isPublic) PUBLIC else PRIVATE, description, extendsConfigs.map(_.name).toArray, transitive, null)
}
object Configurations
{
	def config(name: String) = new Configuration(name)
	def defaultMavenConfigurations = Compile :: Runtime :: Test :: Provided :: System :: Optional :: Sources :: Javadoc :: Nil
	
	lazy val Default = config("default")
	lazy val Compile = config("compile")
	lazy val IntegrationTest = config("it") extend(Compile)
	lazy val Provided = config("provided")
	lazy val Javadoc = config("javadoc")
	lazy val Runtime = config("runtime") extend(Compile)
	lazy val Test = config("test") extend(Runtime) hide
	lazy val Sources = config("sources")
	lazy val System = config("system")
	lazy val Optional = config("optional")
	
	private[sbt] def removeDuplicates(configs: Iterable[Configuration]) = Set(scala.collection.mutable.Map(configs.map(config => (config.name, config)).toSeq: _*).values.toList: _*)
}
/** Interface between Ivy logging and sbt logging. */
private final class IvyLogger(log: Logger) extends MessageLogger
{
	private var progressEnabled = false
	
	def log(msg: String, level: Int)
	{
		import Message.{MSG_DEBUG, MSG_VERBOSE, MSG_INFO, MSG_WARN, MSG_ERR}
		level match
		{
			case MSG_DEBUG | MSG_VERBOSE => debug(msg)
			case MSG_INFO => info(msg)
			case MSG_WARN => warn(msg)
			case MSG_ERR => error(msg)
		}
	}
	def rawlog(msg: String, level: Int)
	{
		log(msg, level)
	}
	import Level.{Debug, Info, Warn, Error}
	def debug(msg: String) = logImpl(msg, Debug)
	def verbose(msg: String) = debug(msg)
	def deprecated(msg: String) = warn(msg)
	def info(msg: String) = logImpl(msg, Info)
	def rawinfo(msg: String) = info(msg)
	def warn(msg: String) = logImpl(msg, Warn)
	def error(msg: String) = logImpl(msg, Error)
	
	private def logImpl(msg: String, level: Level.Value) = log.log(level, msg)
	
	private def emptyList = java.util.Collections.emptyList[T forSome { type T}]
	def getProblems = emptyList
	def getWarns = emptyList
	def getErrors = emptyList

	def clearProblems = ()
	def sumupProblems = ()
	def progress = ()
	def endProgress = ()

	def endProgress(msg: String) = info(msg)
	def isShowProgress = false
	def setShowProgress(progress: Boolean) {}
}
