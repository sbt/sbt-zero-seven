/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

import java.io.File
import scala.collection._
import FileUtilities._
import Project._

trait Project extends Logger with TaskManager with Dag[Project]
{
	def info: ProjectInfo
	/** The tasks that operate on this project. */
	def tasks: Map[String, Task]
	/** The names of available tasks that may be called through `act`.  These include
	* the names of the Tasks in `tasks` and in those of all dependencies.*/
	def taskNames: Iterable[String] = deepTasks.keys.toList
	def deepTasks: Map[String, Project#Task] =
	{
		val tasks = new jcl.TreeMap[String, Project#Task]
		for(dependentProject <- topologicalSort)
			tasks ++= dependentProject.tasks.elements
		tasks
	}
	def subProjects: Map[String, Project] = immutable.Map.empty
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
		if(multiProject && atLevel(Level.Debug))
			showBuildOrder(ordered)
		
		def run(projects: List[Project], actionExists: Boolean): Option[String] =
			projects match
			{
				case Nil => if(actionExists) None else Some("Action '" + name + "' does not exist.")
				case project :: remaining =>
					project.tasks.get(name) match
					{
						case None => run(remaining, actionExists)
						case Some(task) =>
						{
							if(multiProject)
								showProjectHeader(project)
							task.run orElse run(remaining, true)
						}
					}
			}

		run(ordered, false)
	}
	private def showBuildOrder(order: Iterable[Project])
	{
		debug("Project build order:")
		order.foreach(x => debug("    " + x.info.name) )
		debug("")
	}
	private def showProjectHeader(project: Project)
	{
		val projectHeader = "Project " + project.info.name
		info("")
		info(projectHeader)
		info(("=": scala.runtime.RichString) * projectHeader.length)
	}
	
	implicit def path(component: String): Path = info.projectPath / component
	
	def project(path: Path) = getProject(Project.loadProject(path, Nil), path)
	def project(path: Path, dep0: Project, deps: Project*) =
		getProject(Project.loadProject(path, dep0 :: deps.toList), path)
	
	def initializeDirectories() {}
	
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
trait ReflectiveTasks extends Project
{
	def tasks: Map[String, Task] = reflectiveTaskMappings
	def reflectiveTaskMappings : Map[String, Task] = Reflective.reflectiveMappings[Task](this)
}
trait ReflectiveModules extends Project
{
	override def subProjects: Map[String, Project] = reflectiveModuleMappings
	def reflectiveModuleMappings : Map[String, Project] = Reflective.reflectiveMappings[Project](this)
}
trait ReflectiveProject extends ReflectiveModules with ReflectiveTasks

class ParentProject(val info: ProjectInfo, protected val deps: Iterable[Project])
	extends ReflectiveProject with ConsoleLogger
{
	def dependencies = deps ++ subProjects.values.toList
}
object Project
{
	private val log = new ConsoleLogger {}
	log.setLevel(Level.Debug)
	
	val BuilderProjectDirectoryName = "build"
	val ProjectClassName = classOf[Project].getName
	
	def loadProject: Either[String, Project] = loadProject(new File("."), Nil)
	def loadProject(path: Path, deps: Iterable[Project]): Either[String, Project] = loadProject(path.asFile, deps)
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
				val constructor = builderClass.getDeclaredConstructor(classOf[ProjectInfo], classOf[Iterable[Project]])
				val project = constructor.newInstance(info, deps).asInstanceOf[Project]
				if(info.initializeDirectories)
					project.initializeDirectories()
				project
			}
		}
		catch
		{
			case e: Exception =>
			{
				log.trace(e)
				Left("Error loading project: " + e.getMessage)
			}
		}
	}
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
}
