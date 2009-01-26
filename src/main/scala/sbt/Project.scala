/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

import java.io.File
import scala.collection._
import FileUtilities._
import Project._

trait Project extends TaskManager with Dag[Project] with BasicEnvironment
{
	/** The logger for this project definition. */
	final val log: Logger = logImpl
	protected def logImpl: Logger = new BufferedLogger(new ConsoleLogger)
	
	trait ActionOption extends NotNull
	
	/** Basic project information. */
	def info: ProjectInfo
	/** The project name. */
	final def name: String = projectName.value
	/** The project version. */
	final def version: Version = projectVersion.value
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
								project.runAndSaveEnvironment( task.run ) orElse run(remaining)
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
				case x => Some(Set(x: _*).mkString("\n"))
			}
		}
		else
			runSequentially
	}
	/** Logs the list of projects at the debug level.*/
	private def showBuildOrder(order: Iterable[Project])
	{
		log.debug("Project build order:")
		order.foreach(x => log.debug("    " + x.name) )
		log.debug("")
	}
	
	/** Converts a String to a path relative to the project directory of this project. */
	implicit def path(component: String): Path = info.projectPath / component
	/** Converts a String to a simple name filter.  * has the special meaning: zero or more of any character */
	implicit def filter(simplePattern: String): NameFilter = GlobFilter(simplePattern)
	
	/** Loads the project at the given path and declares the project to have the given
	* dependencies.  This method will configure the project according to the
	* project/ directory in the directory denoted by path.*/
	def project(path: Path, deps: Project*): Project = getProject(Project.loadProject(path, deps, Some(this)), path)
	
	/** Loads the project at the given path using the given name and inheriting this project's version.
	* The builder class is the default builder class, sbt.DefaultProject. The loaded project is declared
	* to have the given dependencies. Any project/build/ directory for the project is ignored.*/
	def project(path: Path, name: String, deps: Project*): Project = project(path, name, Project.DefaultBuilderClass, deps: _*)
	
	/** Loads the project at the given path using the given name and inheriting it's version from this project.
	* The Project implementation used is given by builderClass.  The dependencies are declared to be
	* deps. Any project/build/ directory for the project is ignored.*/
	def project[P <: Project](path: Path, name: String, builderClass: Class[P], deps: Project*): P =
	{
		require(builderClass != this.getClass, "Cannot recursively construct projects of same type: " + builderClass.getName)
		project(path, name, info => Project.constructProject(info, builderClass), deps: _*)
	}
	/** Loads the project at the given path using the given name and inheriting it's version from this project.
	* The construct function is used to obtain the Project instance. Any project/build/ directory for the project
	* is ignored.  The project is declared to have the dependencies given by deps.*/
	def project[P <: Project](path: Path, name: String, construct: ProjectInfo => P, deps: Project*): P =
		initialize(construct(ProjectInfo(path.asFile, deps, Some(this))), Some(SetupInfo(name, None, false)))
	
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
	
	/** The path to the file that provides persistence for properties.*/
	final def envBackingPath = info.builderPath / Project.DefaultEnvBackingName
	
	private def getProject(result: LoadResult, path: Path): Project =
		result match
		{
			case LoadSetupDeclined => Predef.error("No project exists at path " + path)
			case LoadSetupError(m) => Predef.error("Error setting up new project at path " + Path + " : " + m)
			case LoadError(m) => Predef.error("Error loading project at path " + path + " : " + m)
			case LoadSuccess(p) => p
		}
	
	/** The property for the project's version. */
	final val projectVersion = property[Version]
	/** The property for the project's name. */
	final val projectName = propertyLocalF[String](NonEmptyStringFormat)
	protected final override def parentEnvironment = info.parent
	
	def runAndSaveEnvironment[T](toRun: => Option[String]): Option[String] =
	{
		try
		{
			val runResult = toRun // execute the action
			val saveResult = saveEnvironment() // always run saveEnvironment 
			runResult orElse saveResult // precendence given to the runResult error if both have an error
		}
		catch
		{
			case e =>
			{
				saveEnvironment()
				log.trace(e)
				Some(e.toString)
			}
		}
	}
	def defaultExcludes = ".svn" | ".cvs"
	/** Short for parent.descendentsExcept(include, defaultExcludes)*/
	def descendents(parent: PathFinder, include: NameFilter) = parent.descendentsExcept(include, defaultExcludes)
	override def toString = "Project " + projectName.get.getOrElse("at " + environmentLabel)
	
	def normalizedName = name.toLowerCase.replaceAll("""\s+""", "-")
}
private[sbt] sealed trait LoadResult extends NotNull
private[sbt] final case class LoadSuccess(p: Project) extends LoadResult
private[sbt] final case class LoadError(message: String) extends LoadResult
private[sbt] final case object LoadSetupDeclined extends LoadResult
private[sbt] final case class LoadSetupError(message: String) extends LoadResult

object Project
{
	val DefaultEnvBackingName = "build.properties"
	val DefaultBuilderClassName = "sbt.DefaultProject"
	val DefaultBuilderClass = Class.forName(DefaultBuilderClassName).asSubclass(classOf[Project])
	
	/** The logger that should be used before the project definition is loaded.*/
	private[sbt] val log = new ConsoleLogger
	log.setLevel(Level.Trace)
	
	/** The name of the directory for project definitions.*/
	val BuilderProjectDirectoryName = "build"
	/** The name of the class that all projects must inherit from.*/
	val ProjectClassName = classOf[Project].getName
	
	/** Loads the project in the current working directory.*/
	private[sbt] def loadProject: LoadResult = checkOutputDirectories(loadProject(new File("."), Nil, None))
	/** Loads the project in the directory given by 'path' and with the given dependencies.*/
	private[sbt] def loadProject(path: Path, deps: Iterable[Project], parent: Option[Project]): LoadResult =
		loadProject(path.asFile, deps, parent)
	/** Loads the project in the directory given by 'projectDirectory' and with the given dependencies.*/
	private[sbt] def loadProject(projectDirectory: File, deps: Iterable[Project], parent: Option[Project]): LoadResult =
	{
		val info = ProjectInfo(projectDirectory, deps, parent)
		ProjectInfo.setup(info, log) match
		{
			case SetupError(message) => LoadSetupError(message)
			case SetupDeclined => LoadSetupDeclined
			case AlreadySetup => loadProject(info, None)
			case setup: SetupInfo => loadProject(info, Some(setup))
		}
	}
	private def loadProject(info: ProjectInfo, setupInfo: Option[SetupInfo]): LoadResult =
	{
		try
		{
			val result =
				for(builderClass <- getProjectDefinition(info).right) yield
					initialize(constructProject(info, builderClass), setupInfo)
			result.fold(LoadError(_), LoadSuccess(_))
		}
		catch
		{
			case ite: java.lang.reflect.InvocationTargetException =>
			{
				val cause =
					if(ite.getCause == null) ite
					else ite.getCause
				errorLoadingProject(cause)
			}
			case nme: NoSuchMethodException => LoadError("Constructor with one argument of type sbt.ProjectInfo required for project definition.")
			case e: Exception => errorLoadingProject(e)
		}
	}
	/** Logs the stack trace and returns an error message in Left.*/
	private def errorLoadingProject(e: Throwable) =
	{
		log.trace(e)
		LoadError("Error loading project: " + e.toString)
	}
	/** Loads the project for the given `info` and represented by an instance of 'builderClass'.*/
	private def constructProject[P <: Project](info: ProjectInfo, builderClass: Class[P]): P =
		builderClass.getConstructor(classOf[ProjectInfo]).newInstance(info)
	/** Checks the project's dependencies, initializes its environment, and possibly its directories.*/
	private def initialize[P <: Project](p: P, setupInfo: Option[SetupInfo]): P =
	{
		p.initializeEnvironment()
		for(setup <- setupInfo)
		{
			p.projectName() = setup.name
			for(v <- setup.version)
				p.projectVersion() = v
			for(errorMessage <- p.saveEnvironment())
				log.error(errorMessage)
			if(setup.initializeDirectories)
				p.initializeDirectories()
		}
		val useName = p.projectName.get.getOrElse("at " + p.info.projectDirectory.getCanonicalPath)
		checkDependencies(useName, p.info.dependencies)
		p
	}
	/** Compiles the project definition classes and returns the project definition class name
	* and the class loader that should be used to load the definition. */
	private def getProjectDefinition(info: ProjectInfo): Either[String, Class[P] forSome { type P <: Project }] =
	{
		val builderProjectPath = info.builderPath / BuilderProjectDirectoryName
		if(builderProjectPath.asFile.isDirectory)
		{
			val builderProject = new BuilderProject(ProjectInfo(builderProjectPath.asFile, Nil, None))
			builderProject.compile.run.toLeft
			{
				builderProject.projectDefinition match
				{
					case Some(definition) =>
					{
						val compileClassPath = Array(builderProject.compilePath.asURL)
						import java.net.URLClassLoader
						val loader = new URLClassLoader(compileClassPath, getClass.getClassLoader)
						val builderClass = Class.forName(definition, false, loader)
						require(classOf[Project].isAssignableFrom(builderClass), "Builder class '" + builderClass + "' does not extend Project.")
						builderClass.asSubclass(classOf[Project])
					}
					case None => DefaultBuilderClass
				}
			}
		}
		else
			Right(DefaultBuilderClass)
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
	private def checkOutputDirectories(result: LoadResult): LoadResult =
		result match
		{
			case LoadSuccess(project) =>
				if(project.shouldCheckOutputDirectories)
					checkOutputDirectoriesImpl(project)
				else
					LoadSuccess(project)
			case x => x
		}
	/** Verifies that output directories of the given project and all of its dependencies are
	* all different.  The 'Project.outputDirectories' method is used to determine a project's
	* output directories. */
	private def checkOutputDirectoriesImpl(project: Project): LoadResult =
	{
		val projects = project.topologicalSort
		import scala.collection.mutable.{HashMap, HashSet, Set}
		val outputDirectories = new HashMap[Path, Set[Project]]
		for(p <- projects; path <- p.outputDirectories)
			outputDirectories.getOrElseUpdate(path, new HashSet[Project]) += p
		val shared = outputDirectories.filter(_._2.size > 1)
		if(shared.isEmpty)
			LoadSuccess(project)
		else
		{
			val sharedString =
			{
				val s =
					for((path, projectsSharingPath) <- shared) yield
						projectsSharingPath.map(_.name).mkString(", ") + " share " + path
				s.mkString("\n\t")
			}
			LoadError("The same directory is used for output for multiple projects:\n\t" + sharedString +
			"\n  (If this is intentional, use 'override def shouldCheckOutputDirectories = false' in your project definition.)")
		}
	}
	
	/** Writes the project name and a separator to the project's log at the info level.*/
	def showProjectHeader(project: Project)
	{
		val projectHeader = "Project " + project.name
		project.log.info("")
		project.log.info(projectHeader)
		project.log.info(("=": scala.runtime.RichString) * projectHeader.length)
	}
}
