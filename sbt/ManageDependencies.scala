package sbt

import java.io.File

import org.apache.ivy.{core, plugins, Ivy}
import core.resolve.ResolveOptions
import core.retrieve.RetrieveOptions
import core.module.descriptor.ModuleDescriptor
import plugins.parser.ModuleDescriptorParser
import plugins.parser.m2.PomModuleDescriptorParser
import plugins.parser.xml.XmlModuleDescriptorParser

object ManageDependencies
{
	val DefaultIvyConfigFilename = "ivysettings.xml"
	val DefaultIvyFilename = "ivy.xml"
	val DefaultMavenFilename = "pom.xml"
	
	def defaultIvyFile(project: Path) = project / DefaultIvyFilename
	def defaultIvyConfiguration(project: Path) = project / DefaultIvyConfigFilename
	def defaultPOM(project: Path) = project / DefaultMavenFilename
	
	def update(projectDirectory: Path, outputPattern: String, managedLibDirectory: Path,
		manager: Manager, validate: Boolean, synchronize: Boolean, log: Logger) =
	{
		val ivy = Ivy.newInstance
		val settings = ivy.getSettings
		settings.setBaseDir(projectDirectory.asFile)
		def readDependencyFile(file: File, parser: ModuleDescriptorParser) =
		{
			try
			{
				Right(parser.parseDescriptor(settings, file.toURI.toURL, validate))
			}
			catch { case e: Exception => log.trace(e); Left("Could not read dependencies: " + e.toString) }
		}
		def readPom(pomFile: File) =
		{
			ivy.configureDefault
			readDependencyFile(pomFile, PomModuleDescriptorParser.getInstance)
		}
		def readIvyFile(ivyFile: File) = readDependencyFile(ivyFile, XmlModuleDescriptorParser.getInstance)
		def configure(configFile: Option[Path])
		{
			configFile match
			{
				case Some(path) => ivy.configure(path.asFile)
				case None => ivy.configureDefault
			}
		}
		def moduleDescriptor =
			manager match
			{
				case MavenManager(configuration, pom) =>
				{
					configure(configuration)
					readPom(pom.asFile)
				}
				case IvyManager(configuration, dependencies) =>
				{
					configure(configuration)
					readIvyFile(dependencies.asFile)
				}
				case AutoDetectManager =>
				{
					val defaultIvyConfigFile = defaultIvyConfiguration(projectDirectory).asFile
					if(defaultIvyConfigFile.canRead)
						ivy.configure(defaultIvyConfigFile)
					else
						ivy.configureDefault
					val defaultPOMFile = defaultPOM(projectDirectory).asFile
					if(defaultPOMFile.canRead)
						readPom(defaultPOMFile)
					else
						readIvyFile(defaultIvyFile(projectDirectory).asFile)
				}
			}
		def processModule(module: ModuleDescriptor) =
		{
			try
			{
				ivy.resolve(module, new ResolveOptions)
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
			catch { case e: Exception => log.trace(e); Some("Could not process dependencies: " + e.toString) }
		}
		
		moduleDescriptor.fold(Some(_), processModule)
	}
}

sealed abstract class Manager extends NotNull
final object AutoDetectManager extends Manager
final case class MavenManager(configuration: Option[Path], pom: Path) extends Manager
final case class IvyManager(configuration: Option[Path], dependencies: Path) extends Manager


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