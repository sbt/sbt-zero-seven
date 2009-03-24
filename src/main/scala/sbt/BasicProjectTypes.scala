/* sbt -- Simple Build Tool
 * Copyright 2009  Mark Harrah
 */
package sbt
 
/** A project that provides a classpath. */
trait ClasspathProject extends Project
{
	/** The local classpath for this project.*/
	def projectClasspath(config: Configuration): PathFinder
	
	/** Returns the classpath of this project and the classpaths of all dependencies for the
	* given configuration.  Specifically, this concatentates projectClasspath(config) for all
	* projects of type ClasspathProject in topologicalSort. */
	def fullClasspath(config: Configuration): PathFinder =
		Path.lazyPathFinder
		{
			val set = new scala.collection.jcl.LinkedHashSet[Path]
			for(project <- topologicalSort)
			{
				project match
				{
					case sp: ClasspathProject => set ++= sp.projectClasspath(config).get
					case _ => ()
				}
			}
			set.toList
		}
}
trait BasicDependencyProject extends BasicManagedProject with UnmanagedClasspathProject with BasicDependencyPaths
{
	/** This returns the classpath for only this project for the given configuration.*/
	def projectClasspath(config: Configuration) = fullUnmanagedClasspath(config) +++ managedClasspath(config)
}
/** A project that provides a directory in which jars can be manually managed.*/
trait UnmanagedClasspathProject extends ClasspathProject
{
	/** The location of the manually managed (unmanaged) dependency directory.*/
	def dependencyPath: Path
	/** The classpath containing all jars in the unmanaged directory. */
	def unmanagedClasspath: PathFinder = descendents(dependencyPath, "*.jar")
	/** The classpath containing all unmanaged classpath elements for the given configuration. This typically includes
	* at least 'unmanagedClasspath'.*/
	def fullUnmanagedClasspath(config: Configuration): PathFinder
}

/** A project that provides automatic dependency management.*/
trait ManagedProject extends ClasspathProject
{
	trait ManagedOption extends ActionOption
	final class ManagedFlagOption extends ManagedOption
	/** An update option that specifies that unneeded files should be pruned from the managed library directory
	* after updating. */
	final val Synchronize = new ManagedFlagOption
	/** An update option that specifies that Ivy should validate configurations.*/
	final val Validate = new ManagedFlagOption
	/** An update option that puts Ivy into a quieter logging mode.*/
	final val QuietUpdate = new ManagedFlagOption
	/** An update option that adds the scala-tools.org releases repository to the set of resolvers, unless
	* no inline repositories are present and an ivysettings.xml file is present.*/
	final val AddScalaToolsReleases = new ManagedFlagOption
	/** An update option that specifies that an error should be generated if no inline dependencies, resolvers,
	* XML file, or Ivy or Maven configuration files are present.*/
	final val ErrorIfNoConfiguration = new ManagedFlagOption
	/** An update option that explicitly specifies the dependency manager to use.  This can be used to
	* override the default precendence. */
	final case class LibraryManager(m: Manager) extends ManagedOption
	/** An update option that overrides the default Ivy cache location. */
	final case class CacheDirectory(dir: Path) extends ManagedOption
	
	private def withConfigurations(outputPattern: String, managedDependencyPath: Path, options: Seq[ManagedOption])
		(doWith: (IvyConfiguration, UpdateConfiguration) => Option[String]) =
	{
		var synchronize = false
		var validate = false
		var quiet = false
		var addScalaTools = false
		var errorIfNoConfiguration = false
		var manager: Manager = new AutoDetectManager(projectID)
		var cacheDirectory: Option[Path] = None
		for(option <- options)
		{
			option match
			{
				case Synchronize => synchronize = true
				case Validate => validate = true
				case LibraryManager(m) => manager = m
				case QuietUpdate => quiet = true
				case AddScalaToolsReleases => addScalaTools = true
				case ErrorIfNoConfiguration => errorIfNoConfiguration = true
				case CacheDirectory(dir) => cacheDirectory = Some(dir)
				case _ => log.warn("Ignored unknown managed option " + option)
			}
		}
		val ivyPaths = new IvyPaths(info.projectPath, managedDependencyPath, cacheDirectory)
		val ivyFlags = new IvyFlags(validate, addScalaTools, errorIfNoConfiguration)
		val ivyConfiguration = new IvyConfiguration(ivyPaths, manager, ivyFlags, log)
		val updateConfiguration = new UpdateConfiguration(outputPattern, synchronize, quiet)
		doWith(ivyConfiguration, updateConfiguration)
	}
	private def withIvyTask(doTask: => Option[String]) =
		task
		{
			try { doTask }
			catch
			{
				case e: NoClassDefFoundError =>
					log.trace(e)
					Some("Apache Ivy is required for dependency management (" + e.toString + ")")
			}
		}
	def updateTask(outputPattern: String, managedDependencyPath: Path, options: ManagedOption*): Task =
		updateTask(outputPattern, managedDependencyPath, options)
	def updateTask(outputPattern: String, managedDependencyPath: Path, options: => Seq[ManagedOption]) =
		withIvyTask(withConfigurations(outputPattern, managedDependencyPath, options)(ManageDependencies.update))
		
	def publishTask(publishConfig: PublishConfiguration, options: => Seq[ManagedOption]) =
	{
		import publishConfig._
		withIvyTask(withConfigurations("", managedDependencyPath, options) { (ivyConf, ignore) =>
			ManageDependencies.publish(ivyConf, resolverName, srcArtifactPatterns, deliveredPattern, configurations) } )
	}
	def deliverTask(deliverConfig: PublishConfiguration, options: => Seq[ManagedOption]) =
	{
		import deliverConfig._
		withIvyTask(withConfigurations("", managedDependencyPath, options) { (ivyConf, updateConf) =>
			ManageDependencies.deliver(ivyConf, updateConf, status, deliveredPattern, extraDependencies, configurations) } )
	}
	def makePomTask(output: Path, extraDependencies: Iterable[ModuleID], options: => Seq[ManagedOption]) =
		withIvyTask(withConfigurations("", managedDependencyPath: Path, options) { (ivyConf, ignore) =>
			ManageDependencies.makePom(ivyConf, extraDependencies, output.asFile) })
		
	def cleanCacheTask(managedDependencyPath: Path, options: => Seq[ManagedOption]) =
		withIvyTask(withConfigurations("", managedDependencyPath, options) { (ivyConf, ignore) => ManageDependencies.cleanCache(ivyConf) })
		
	def cleanLibTask(managedDependencyPath: Path) = task { FileUtilities.clean(managedDependencyPath.get, log) }
	
	def projectID: ModuleID = ModuleID(organization, normalizedName, version.toString, None)
	def managedDependencyPath: Path
	/** The managed classpath for the given configuration, using the default configuration if this configuration
	* does not exist in the managed library directory.*/
	final def managedClasspath(config: Configuration): PathFinder = managedClasspath(config, true)
	/** The managed classpath for the given configuration.  If 'useDefaultFallback' is true, the default configuration
	* will be used if the configuration does not exist in the managed library directory.*/
	def managedClasspath(config: Configuration, useDefaultFallback: Boolean): PathFinder =
	{
		val configDirectory = managedDependencyPath / config.toString
		val useDirectory =
			if(configDirectory.asFile.exists)
				configDirectory
			else if(useDefaultFallback)
				managedDependencyPath / Configurations.Default.toString
			else
				Path.emptyPathFinder
		descendents(useDirectory, "*.jar")
	}
	
	import StringUtilities.nonEmpty
	implicit def toGroupID(groupID: String): GroupID =
	{
		nonEmpty(groupID, "Group ID")
		new GroupID(groupID)
	}
	implicit def toRepositoryName(name: String): RepositoryName =
	{
		nonEmpty(name, "Repository name")
		new RepositoryName(name)
	}
	implicit def moduleIDConfigurable(m: ModuleID): ModuleIDConfigurable =
	{
		require(m.configurations.isEmpty, "Configurations already specified for module " + m)
		new ModuleIDConfigurable(m)
	}
	
	def config(name: String) = new Configuration(name)
}
/** This class groups required configuration for the deliver and publish tasks. */
trait PublishConfiguration extends NotNull
{
	/** The name of the resolver to which publishing should be done.*/
	def resolverName: String
	/** The Ivy pattern used to determine the delivered Ivy file location.  An example is
	* (outputPath / "[artifact]-[revision].[ext]").relativePath */
	def deliveredPattern: String
	/** Ivy patterns used to find artifacts for publishing.  An example pattern is
	* (outputPath / "[artifact]-[revision].[ext]").relativePath */
	def srcArtifactPatterns: Iterable[String]
	/** Additional dependencies to include for delivering/publishing only.  These are typically dependencies on
	* subprojects. */
	def extraDependencies: Iterable[ModuleID]
	/** The status to use when delivering or publishing.  This might be "release" or "integration" or another valid Ivy status. */
	def status: String
	/**  The configurations to include in the publish/deliver action: specify none for all configurations. */
	def configurations: Option[Iterable[Configuration]]
}
	
trait BasicManagedProject extends ManagedProject with ReflectiveManagedProject
{
	/** The dependency manager that represents inline declarations.  The default manager packages the information
	* from 'ivyXML', 'projectID', 'repositories', and 'libraryDependencies' and does not typically need to be
	* be overridden. */
	def manager = new SimpleManager(ivyXML, true, projectID, repositories, ivyConfigurations, defaultConfiguration, libraryDependencies.toList: _*)
	
	/** The pattern for Ivy to use when retrieving dependencies into the local project.  Classpath management
	* depends on the first directory being [conf] and the extension being [ext].*/
	def outputPattern = "[conf]/[artifact](-[revision]).[ext]"
	/** Override this to specify the publications, configurations, and/or dependencies sections of an Ivy file.
	* See http://code.google.com/p/simple-build-tool/wiki/LibraryManagement for details.*/
	def ivyXML: scala.xml.NodeSeq = scala.xml.NodeSeq.Empty
	/** The base options passed to the 'update' action. */
	def baseUpdateOptions = Validate :: Synchronize :: QuietUpdate :: AddScalaToolsReleases :: Nil
	override def ivyConfigurations: Iterable[Configuration] =
	{
		val reflective = reflectiveIvyConfigurations
		if(useMavenConfigurations)
		{
			val base = Configurations.defaultMavenConfigurations ++ reflective
			val allConfigurations =
				if(useIntegrationTestConfiguration)
					base ++ List(Configurations.IntegrationTest)
				else
					base
			Configurations.removeDuplicates(allConfigurations)
		}
		else
			reflective
	}
	def useIntegrationTestConfiguration = false
	def defaultConfiguration = if(useMavenConfigurations) Some(Configurations.Compile) else None
	def useMavenConfigurations = false
	/** The options provided to the 'update' action.  This is by default the options in 'baseUpdateOptions'.
	* If 'manager' has any dependencies, resolvers, or inline Ivy XML (which by default happens when inline
	* dependency management is used), it is passed as the dependency manager.*/
	def updateOptions: Seq[ManagedOption] =
	{
		val m = manager
		if(m.dependencies.isEmpty && m.resolvers.isEmpty && ivyXML.isEmpty)
			baseUpdateOptions
		else
			LibraryManager(m) :: baseUpdateOptions
	}
	def includeProvidedWithCompile = true
	def defaultConfigurationExtensions = true
	/** Includes the Provided configuration on the Compile classpath.  This can be overridden by setting
	* includeProvidedWithCompile to false.*/
	override def managedClasspath(config: Configuration, useDefaultFallback: Boolean) =
	{
		import Configurations.{Compile, Provided, Runtime, Test}
		val superClasspath = super.managedClasspath(config, useDefaultFallback)
		if(config == Compile && includeProvidedWithCompile)
			superClasspath +++ super.managedClasspath(Provided, false)
		else if(defaultConfigurationExtensions && config == Runtime)
			superClasspath +++ super.managedClasspath(Compile, false)
		else if(defaultConfigurationExtensions && config == Test)
			superClasspath +++ super.managedClasspath(Compile, false) +++ super.managedClasspath(Runtime, false)
		else
			superClasspath
	}
}

object StringUtilities
{
	def nonEmpty(s: String, label: String)
	{
		require(s.trim.length > 0, label + " cannot be empty.")
	}
}
import StringUtilities.nonEmpty
final class GroupID private[sbt] (groupID: String) extends NotNull
{
	def % (artifactID: String) =
	{
		nonEmpty(artifactID, "Artifact ID")
		new GroupArtifactID(groupID, artifactID)
	}
}
final class GroupArtifactID private[sbt] (groupID: String, artifactID: String) extends NotNull
{
	def % (revision: String): ModuleID =
	{
		nonEmpty(revision, "Revision")
		ModuleID(groupID, artifactID, revision, None)
	}
}
final class ModuleIDConfigurable private[sbt] (moduleID: ModuleID) extends NotNull
{
	def % (configurations: String): ModuleID =
	{
		nonEmpty(configurations, "Configurations")
		import moduleID._
		ModuleID(organization, name, revision, Some(configurations))
	}
}
final class RepositoryName private[sbt] (name: String) extends NotNull
{
	def at (location: String) =
	{
		nonEmpty(location, "Repository location")
		new MavenRepository(name, location)
	}
}

import scala.collection.{Map, mutable}
/** A Project that determines its tasks by reflectively finding all vals with a type
* that conforms to Task.*/
trait ReflectiveTasks extends Project
{
	def tasks: Map[String, Task] = reflectiveTaskMappings
	def reflectiveTaskMappings : Map[String, Task] = Reflective.reflectiveMappings[Task](this)
}
/** A Project that determines its method tasks by reflectively finding all vals with a type
* that conforms to MethodTask.*/
trait ReflectiveMethods extends Project
{
	def methods: Map[String, MethodTask] = reflectiveMethodMappings
	def reflectiveMethodMappings : Map[String, MethodTask] = Reflective.reflectiveMappings[MethodTask](this)
}
/** A Project that determines its dependencies on other projects by reflectively
* finding all vals with a type that conforms to Project.*/
trait ReflectiveModules extends Project
{
	override def subProjects: Map[String, Project] = reflectiveModuleMappings
	def reflectiveModuleMappings : Map[String, Project] = Reflective.reflectiveMappings[Project](this)
}
/** A Project that determines its dependencies on other projects by reflectively
* finding all vals with a type that conforms to Project and determines its tasks
* by reflectively finding all vals with a type that conforms to Task.*/
trait ReflectiveProject extends ReflectiveModules with ReflectiveTasks with ReflectiveMethods

/** This Project subclass is used to contain other projects as dependencies.*/
class ParentProject(val info: ProjectInfo) extends BasicDependencyProject
{
	def dependencies = info.dependencies ++ subProjects.values.toList
	/** The directories to which a project writes are listed here and is used
	* to check a project and its dependencies for collisions.*/
	override def outputDirectories = managedDependencyPath :: Nil
	def fullUnmanagedClasspath(config: Configuration) = unmanagedClasspath
}

object Reflective
{
	def reflectiveMappings[T](obj: AnyRef)(implicit m: scala.reflect.Manifest[T]): Map[String, T] =
	{
		val mappings = new mutable.OpenHashMap[String, T]
		for ((name, value) <- ReflectUtilities.allVals[T](obj))
			mappings(ReflectUtilities.transformCamelCase(name, '-')) = value
		mappings
	}
}

/** A Project that determines its library dependencies by reflectively finding all vals with a type
* that conforms to ModuleID.*/
trait ReflectiveLibraryDependencies extends ManagedProject
{
	def excludeIDs: Iterable[ModuleID] = projectID :: Nil
	def libraryDependencies: Set[ModuleID] = reflectiveLibraryDependencies
	def reflectiveLibraryDependencies : Set[ModuleID] = Set(Reflective.reflectiveMappings[ModuleID](this).values.toList: _*) -- excludeIDs
}

trait ReflectiveConfigurations extends Project
{
	def ivyConfigurations: Iterable[Configuration] = reflectiveIvyConfigurations
	def reflectiveIvyConfigurations: Set[Configuration] = Configurations.removeDuplicates(Reflective.reflectiveMappings[Configuration](this).values.toList)
}

/** A Project that determines its library dependencies by reflectively finding all vals with a type
* that conforms to ModuleID.*/
trait ReflectiveRepositories extends Project
{
	def repositories: Set[Resolver] = reflectiveRepositories
	def reflectiveRepositories: Set[Resolver] = Set(Reflective.reflectiveMappings[Resolver](this).values.toList: _*)
}

trait ReflectiveManagedProject extends ReflectiveProject with ReflectiveRepositories with ReflectiveLibraryDependencies with ReflectiveConfigurations