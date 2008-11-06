/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

import java.io.File
import scala.collection._
import FileUtilities._
import Project._

trait Project extends TaskManager with Dag[Project]
{
	/** The logger for this project definition. */
	final val log: Logger = logImpl
	protected def logImpl: Logger = new BufferedLogger(new ConsoleLogger)
	
	/** Basic project information. */
	def info: ProjectInfo
	/** The tasks declared on this project. */
	def tasks: Map[String, Task]
	/** The names of all available tasks that may be called through `act`.  These include
	* the names of the Tasks in `tasks` and those of all dependencies.*/
	def taskNames: Iterable[String] = deepTasks.keys.toList
	/** A description of all available tasks in this project and all dependencies.  If there
	* are different tasks with the same name, only one will be included. */
	def taskList: String =
	{
		val buffer = new StringBuilder
		for((name, task) <- deepTasks)
			buffer.append("\t" + name + task.description.map(x => ": " + x).getOrElse("") + "\n")
		buffer.toString
	}
	/** Combines the task maps of this project and all dependencies.*/
	private[sbt] def deepTasks: Map[String, Project#Task] =
	{
		val tasks = new jcl.TreeMap[String, Project#Task]
		for(dependentProject <- topologicalSort)
			tasks ++= dependentProject.tasks.elements
		tasks
	}
	/** A map of names to projects for all subprojects of this project.  These are typically explicitly
	* specified for the project and are different from those specified in the project constructor. The
	* main use within sbt is in ParentProject.*/
	def subProjects: Map[String, Project] = immutable.Map.empty
	/** The name of this project and the names of all subprojects/dependencies, transitively.*/
	def projectNames: Iterable[String] =
	{
		val names = new mutable.HashSet[String]
		names ++= subProjects.keys
		for(dependentProject <- topologicalSort)
			names ++= dependentProject.tasks.keys
		names.toList
	}
	
	/** Executes the task with the given name.  This involves executing the task for all
	* project dependencies (transitive) and then for this project.  Not every dependency
	* must define a task with the given name.  If this project and all dependencies
	* do not define a task with the given name, an error is generated indicating this.*/
	def act(name: String): Option[String] =
	{
		val ordered = topologicalSort
		val multiProject = ordered.length > 1
		
		def runSequentially =
		{
			if(multiProject && log.atLevel(Level.Debug))
				showBuildOrder(ordered)
			
			def run(projects: List[Project]): Option[String] =
				projects match
				{
					case Nil => None
					case project :: remaining =>
						project.tasks.get(name) match
						{
							case None => run(remaining)
							case Some(task) =>
							{
								if(multiProject)
									showProjectHeader(project)
								task.run orElse run(remaining)
							}
						}
				}
			run(ordered)
		}

		val definedTasks = ordered.flatMap(_.tasks.get(name).toList)
		if(definedTasks.isEmpty)
			Some("Action '" + name + "' does not exist.")
		else if(!tasks.contains(name) && definedTasks.forall(_.interactive))
			Some("Action '" + name + "' is not defined on this project and is declared as interactive on all dependencies.")
		else if(multiProject && parallelExecution)
		{
			ParallelRunner.run(this, name, Runtime.getRuntime.availableProcessors) match
			{
				case Nil => None
				case x => Some(x.mkString("\n"))
			}
		}
		else
			runSequentially
	}
	/** Logs the list of projects at the debug level.*/
	private def showBuildOrder(order: Iterable[Project])
	{
		log.debug("Project build order:")
		order.foreach(x => log.debug("    " + x.info.name) )
		log.debug("")
	}
	
	/** Converts a String to a path relative to the project directory of this project. */
	implicit def path(component: String): Path = info.projectPath / component
	/** Converts a String to a simple name filter.  * has the special meaning: zero or more of any character */
	implicit def filter(simplePattern: String): NameFilter = new GlobFilter(simplePattern)
	
	/** Loads the project at the given path and declares the project to have the given
	* dependencies.  This method will configure the project according to 
	* metadata/info in the directory denoted by path.*/
	def project(path: Path, deps: Project*): Project = getProject(Project.loadProject(path, deps), path)
	/** Loads the project at the given path using the given name and inheriting this project's version.
	* The builder class is the default builder class, sbt.DefaultProject. The loaded project is declared
	* to have the given dependencies. Any metadata/info file for the project is ignored.*/
	def project(path: Path, name: String, deps: Project*): Project = 
		project(path, name, ProjectInfo.DefaultBuilderClass, deps: _*)
	/** Loads the project at the given path using the given name and inheriting it's version from this project.
	* The Project implementation used is given by builderClass.  The dependencies are declared to be
	* deps. Any metadata/info file for the project is ignored.*/
	def project[P <: Project](path: Path, name: String, builderClass: Class[P], deps: Project*): Project =
	{
		checkDependencies(name, deps)
		require(builderClass != this.getClass, "Cannot recursively construct projects of same type: " + builderClass.getName)
		val newInfo = new ProjectInfo(name, info.currentVersion, builderClass.getName, path.asFile)(false)
		Project.loadProject(newInfo, builderClass, deps)
	}
	/** Loads the project at the given path using the given name and inheriting it's version from this project.
	* The construct function is used to obtain the Project instance. Any metadata/info file for the project
	* is ignored.  The project is declared to have no dependencies.*/
	def project(path: Path, name: String, construct: ProjectInfo => Project): Project =
	{
		val newInfo = new ProjectInfo(name, info.currentVersion, "", path.asFile)(false)
		construct(newInfo)
	}
	/** Loads the project at the given path using the given name and inheriting it's version from this project.
	* The construct function is used to obtain the Project instance. Any metadata/info file for the project
	* is ignored.  The construct function is passed the dependencies given by deps.*/
	def project(path: Path, name: String, construct: (ProjectInfo, Iterable[Project]) => Project, deps: Iterable[Project]): Project =
	{
		checkDependencies(name, deps)
		val newInfo = new ProjectInfo(name, info.currentVersion, "", path.asFile)(false)
		construct(newInfo, deps)
	}
	
	/** Initializes the project directories when a user has requested that sbt create a new project.*/
	def initializeDirectories() {}
	/** True if projects should be run in parallel, false if they should run sequentially.
	*  This only has an effect for multi-projects.*/
	def parallelExecution = false
	
	/** True if a project and its dependencies should be checked to ensure that their
	* output directories are not the same, false if they should not be checked. */
	def shouldCheckOutputDirectories = true
	
	/** The list of directories to which this project writes.  This is used to verify that multiple
	* projects have not been defined with the same output directories. */
	def outputDirectories: Iterable[Path] = Nil
	
	private def getProject(e: Either[String,Project], path: Path): Project =
		e.fold(m => Predef.error("Error getting project " + path + " : " + m), x => x)
}
object Reflective
{
	def reflectiveMappings[T](obj: AnyRef)(implicit m: scala.reflect.Manifest[T]): Map[String, T] =
	{
		val mappings = new mutable.OpenHashMap[String, T]
		for ((name, value) <- ReflectUtilities.allVals[T](obj))
			mappings(ReflectUtilities.camelCaseToActionName(name)) = value
		mappings
	}
}
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
class ParentProject(val info: ProjectInfo, protected val deps: Iterable[Project])
	extends ReflectiveProject
{
	def dependencies = deps ++ subProjects.values.toList
}
object Project
{
	/** The logger that should be used before the project definition is loaded.*/
	private val log = new ConsoleLogger
	log.setLevel(Level.Trace)
	
	/** The name of the directory for project definitions.*/
	val BuilderProjectDirectoryName = "build"
	/** The name of the class that all projects must inherit from.*/
	val ProjectClassName = classOf[Project].getName
	
	/** Loads the project in the current working directory.*/
	def loadProject: Either[String, Project] = loadProject(new File("."), Nil).right.flatMap(checkOutputDirectories)
	/** Loads the project in the directory given by 'path' and with the given dependencies.*/
	def loadProject(path: Path, deps: Iterable[Project]): Either[String, Project] = loadProject(path.asFile, deps)
	/** Loads the project in the directory given by 'projectDirectory' and with the given dependencies.*/
	private def loadProject(projectDirectory: File, deps: Iterable[Project]): Either[String, Project] =
	{
		try
		{
			for(info <- ProjectInfo.load(projectDirectory, log).right;
				classAndLoader <- getProjectDefinition(info).right) yield
			{
				val (builderClassName, loader) = classAndLoader
				val builderClass = Class.forName(builderClassName, false, loader)
				require(classOf[Project].isAssignableFrom(builderClass), "Builder class '" + builderClass + "' does not extend Project.")
				loadProject(info, builderClass.asSubclass(classOf[Project]), deps)
			}
		}
		catch
		{
			case e: Exception =>
			{
				log.trace(e)
				Left("Error loading project: " + e.toString)
			}
		}
	}
	/** Loads the project for the given `info`, represented by an instance of 'builderClass', and
	* with the given dependencies.*/
	private def loadProject[P <: Project](info: ProjectInfo, builderClass: Class[P], deps: Iterable[Project]): Project =
	{
		checkDependencies(info.name, deps)
		val constructor = builderClass.getConstructor(classOf[ProjectInfo], classOf[Iterable[Project]])
		val project = constructor.newInstance(info, deps)
		if(info.initializeDirectories)
			project.initializeDirectories()
		project
	}
	/** Compiles the project definition classes and returns the project definition class name
	* and the class loader that should be used to load the definition. */
	private def getProjectDefinition(info: ProjectInfo): Either[String, (String, ClassLoader)] =
	{
		val builderProjectPath = info.builderPath / BuilderProjectDirectoryName
		if(builderProjectPath.asFile.isDirectory)
		{
			val builderInfo = ProjectInfo(info.name + " Builder", info.currentVersion,
				classOf[BuilderProject].getName, builderProjectPath.asFile)(false)
			val builderProject = new BuilderProject(builderInfo)
			builderProject.compile.run.toLeft
			{
				val compileClassPath = Array(builderProject.compilePath.asURL)
				import java.net.URLClassLoader
				val loader = new URLClassLoader(compileClassPath, getClass.getClassLoader)
				builderProject.projectDefinition match
				{
					case Some(definition) => (definition, loader)
					case None => (info.builderClassName, loader)
				}
			}
		}
		else
			Right((info.builderClassName, getClass.getClassLoader))
	}
	/** Verifies that the given list of project dependencies contains no nulls.  The
	* String argument should be the project name with the dependencies.*/
	private def checkDependencies(forProject: String, deps: Iterable[Project])
	{
		for(nullDep <- deps.find(_ == null))
		{
			log.error("Project " + forProject + " had a null dependency.  This is probably an initialization problem and might be due to a circular dependency.")
			throw new RuntimeException("Null dependency in project " + forProject)
		}
	}
	/** Verifies that output directories of the given project and all of its dependencies are
	* all different.  No verification is done if the project overrides
	* 'shouldCheckOutputDirectories' to be false. The 'Project.outputDirectories' method is
	* used to determine a project's output directories. */
	private def checkOutputDirectories(project: Project): Either[String, Project] =
	{
		if(project.shouldCheckOutputDirectories)
			checkOutputDirectoriesImpl(project).toLeft(project)
		else
			Right(project)
	}
	/** Verifies that output directories of the given project and all of its dependencies are
	* all different.  The 'Project.outputDirectories' method is used to determine a project's
	* output directories. */
	private def checkOutputDirectoriesImpl(project: Project): Option[String] =
	{
		val projects = project.topologicalSort
		import scala.collection.mutable.{HashMap, HashSet, Set}
		val outputDirectories = new HashMap[Path, Set[Project]]
		for(p <- projects; path <- p.outputDirectories)
			outputDirectories.getOrElseUpdate(path, new HashSet[Project]) += p
		val shared = outputDirectories.filter(_._2.size > 1)
		if(shared.isEmpty)
			None
		else
		{
			val sharedString =
			{
				val s =
					for((path, projectsSharingPath) <- shared) yield
						projectsSharingPath.map(_.info.name).mkString(", ") + " share " + FileUtilities.printableFilename(path.asFile)
				s.mkString("\n\t")
			}
			Some("The same directory is used for output for multiple projects:\n\t" + sharedString +
			"\n  (If this is intentional, use 'override def shouldCheckOutputDirectories = false' in your project definition.)")
		}
	}
	
	/** Writes the project name and a separator to the project's log at the info level.*/
	def showProjectHeader(project: Project)
	{
		val projectHeader = "Project " + project.info.name
		project.log.info("")
		project.log.info(projectHeader)
		project.log.info(("=": scala.runtime.RichString) * projectHeader.length)
	}
}
