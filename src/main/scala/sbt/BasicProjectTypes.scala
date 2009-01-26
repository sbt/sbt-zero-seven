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

	import scala.reflect.Manifest
	protected[sbt] def projectPathConcatenate[T <: Project](getPath: T => PathFinder)(implicit mf: Manifest[T]): PathFinder =
	{
		(Path.emptyPathFinder /: topologicalSort) { (current, project) =>
			if(Manifest.classType(project.getClass) <:< mf)
				getPath(project.asInstanceOf[T]) +++ current
			else
				current
		}
	}
}
/** A project that provides a directory in which jars can be manually managed.*/
trait UnmanagedClasspathProject extends ClasspathProject
{
	/** The location of the manually managed (unmanaged) dependency directory.*/
	def dependencyPath: Path
	/** The classpath containing all jars in the unmanaged directory. */
	def unmanagedClasspath: PathFinder = descendents(dependencyPath, "*.jar")
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
	
	private def withConfigurations(outputPattern: String, managedDependencyPath: Path, options: Seq[ManagedOption])
		(doWith: (IvyConfiguration, UpdateConfiguration) => Option[String]) =
	{
		var synchronize = false
		var validate = false
		var quiet = false
		var addScalaTools = false
		var errorIfNoConfiguration = false
		var manager: Manager = AutoDetectManager
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
				case _ => log.warn("Ignored unknown managed option " + option)
			}
		}
		val ivyConfiguration = IvyConfiguration(info.projectPath, managedDependencyPath, manager, validate,
			addScalaTools, errorIfNoConfiguration, log)
		val updateConfiguration = UpdateConfiguration(outputPattern, synchronize, quiet)
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
		
	def cleanCacheTask(managedDependencyPath: Path, options: => Seq[ManagedOption]) =
		withIvyTask(withConfigurations("", managedDependencyPath, options) { (ivyConf, ignore) => ManageDependencies.cleanCache(ivyConf) })
		
	def cleanLibTask(managedDependencyPath: Path) = task { FileUtilities.clean(managedDependencyPath.get, log) }
	
	def projectID: ModuleID = ModuleID(normalizedName, normalizedName, version.toString, None)
	def managedDependencyPath: Path
	def managedClasspath(config: Configuration): PathFinder = managedClasspath(config, true)
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
		GroupID(groupID)
	}
	implicit def toRepositoryName(name: String): RepositoryName =
	{
		nonEmpty(name, "Repository name")
		RepositoryName(name)
	}
	implicit def moduleIDConfigurable(m: ModuleID): ModuleIDConfigurable =
	{
		require(m.configurations.isEmpty, "Configurations already specified for module " + m)
		ModuleIDConfigurable(m)
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
final case class GroupID(groupID: String) extends NotNull
{
	def % (artifactID: String) =
	{
		nonEmpty(artifactID, "Artifact ID")
		GroupArtifactID(groupID, artifactID)
	}
}
final case class GroupArtifactID(groupID: String, artifactID: String) extends NotNull
{
	def % (revision: String): ModuleID =
	{
		nonEmpty(revision, "Revision")
		ModuleID(groupID, artifactID, revision, None)
	}
}
final case class ModuleIDConfigurable(moduleID: ModuleID) extends NotNull
{
	def % (configurations: String): ModuleID =
	{
		nonEmpty(configurations, "Configurations")
		import moduleID._
		ModuleID(organization, name, revision, Some(configurations))
	}
}
final case class RepositoryName(name: String) extends NotNull
{
	def at (location: String) =
	{
		nonEmpty(location, "Repository location")
		MavenRepository(name, location)
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
trait ReflectiveProject extends ReflectiveModules with ReflectiveTasks

/** This Project subclass is used to contain other projects as dependencies.*/
class ParentProject(val info: ProjectInfo) extends UnmanagedClasspathProject with ReflectiveManagedProject
	with ManagedProject with BasicDependencyPaths
{
	def dependencies = info.dependencies ++ subProjects.values.toList
	/** The directories to which a project writes are listed here and is used
	* to check a project and its dependencies for collisions.*/
	override def outputDirectories = managedDependencyPath :: Nil
	def projectClasspath(config: Configuration) = unmanagedClasspath +++ managedClasspath(config)
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

/** A Project that determines its library dependencies by reflectively finding all vals with a type
* that conforms to ModuleID.*/
trait ReflectiveRepositories extends Project
{
	def repositories: Set[Resolver] = reflectiveRepositories
	def reflectiveRepositories: Set[Resolver] = Set(Reflective.reflectiveMappings[Resolver](this).values.toList: _*)
}

trait ReflectiveManagedProject extends ReflectiveProject with ReflectiveRepositories with ReflectiveLibraryDependencies