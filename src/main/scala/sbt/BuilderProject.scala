/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah, David MacIver
 */
package sbt

import BasicProjectPaths._

sealed abstract class InternalProject extends Project
{
	override final def historyPath = None
	override def tasks: Map[String, Task] = Map.empty
	override final protected def disableCrossPaths = false
	override final def shouldCheckOutputDirectories = false
}
private sealed abstract class BasicBuilderProject extends InternalProject with SimpleScalaProject
{
	def sourceFilter = "*.scala" | "*.java"
	def jarFilter = "*.jar"
	def compilePath = outputPath / DefaultMainCompileDirectoryName
	def mainResourcesPath = path(DefaultResourcesDirectoryName)
	def dependencyPath = path(DefaultDependencyDirectoryName)
	def libraries = descendents(dependencyPath, jarFilter)
	override final def dependencies = Nil
	
	def projectClasspath = compilePath +++ libraries +++ sbtJarPath
	def sbtJarPath = Path.lazyPathFinder { Path.fromFile(FileUtilities.sbtJar) :: Nil }
		
	abstract class BuilderCompileConfiguration extends AbstractCompileConfiguration
	{
		def projectPath = info.projectPath
		def log = BasicBuilderProject.this.log
		def options = (Deprecation :: Unchecked :: Nil).map(_.asString)
		def javaOptions = Nil
		def maxErrors = ScalaProject.DefaultMaximumCompileErrors
		def compileOrder = CompileOrder.Mixed
	}
	def definitionCompileConfiguration =
		new BuilderCompileConfiguration
		{
			def label = "builder"
			def sources = (info.projectPath * sourceFilter) +++ path(DefaultSourceDirectoryName).descendentsExcept(sourceFilter, defaultExcludes)
			def outputDirectory = compilePath
			def classpath = projectClasspath
			def analysisPath = outputPath / DefaultMainAnalysisDirectoryName
		}
		
	def tpe: String

	val definitionCompileConditional = new BuilderCompileConditional(definitionCompileConfiguration)
	final class BuilderCompileConditional(config: BuilderCompileConfiguration) extends AbstractCompileConditional(config)
	{
		type AnalysisType = BuilderCompileAnalysis
		override protected def constructAnalysis(analysisPath: Path, projectPath: Path, log: Logger) =
			new BuilderCompileAnalysis(analysisPath, projectPath, log)
		override protected def execute(cAnalysis: ConditionalAnalysis): Option[String] =
		{
			if(cAnalysis.dirtySources.isEmpty)
				None
			else
			{
				definitionChanged()
				val oldLevel = log.getLevel
				log.setLevel(Level.Info)
				log.info("Recompiling " + tpe + " definition...")
				log.info("\t" + cAnalysis.toString)
				log.setLevel(oldLevel)
				super.execute(cAnalysis)
			}
		}
		protected def analysisCallback: AnalysisCallback =
			new BasicAnalysisCallback(info.projectPath, List(Project.ProjectClassName), analysis)
			{
				def foundApplication(sourcePath: Path, className: String)  {}
				def foundSubclass(sourcePath: Path, subclassName: String, superclassName: String, isModule: Boolean)
				{
					if(superclassName == Project.ProjectClassName && !isModule)
					{
						log.debug("Found " + tpe + " definition " + subclassName)
						analysis.addProjectDefinition(sourcePath, subclassName)
					}
				}
			}
	}
	protected def definitionChanged() {}
	lazy val compile = compileTask
	def compileTask = task { definitionCompileConditional.run }
	
	def projectDefinition: Either[String, Option[String]] =
	{
		definitionCompileConditional.analysis.allProjects.toList match
		{
			case Nil => 
				log.debug("No " + tpe + " definitions detected using default project.")
				Right(None)
			case singleDefinition :: Nil => Right(Some(singleDefinition))
			case multipleDefinitions =>Left(multipleDefinitions.mkString("Multiple " + tpe + " definitions detected: \n\t","\n\t","\n"))
		}
	}
	override final def methods = Map.empty
}
/** The project definition used to build project definitions. */
private final class BuilderProject(val info: ProjectInfo, val pluginPath: Path, override protected val logImpl: Logger) extends BasicBuilderProject
{
	private lazy val pluginProject =
	{
		if(pluginPath.exists)
			Some(new PluginBuilderProject(ProjectInfo(pluginPath.asFile, Nil, None)))
		else
			None
	}
	override def projectClasspath = super.projectClasspath +++ pluginProject.map(_.pluginClasspath).getOrElse(Path.emptyPathFinder)
	def tpe = "project"

	override def compileTask = super.compileTask dependsOn(pluginProject.map(_.syncPlugins).toList : _*)
		
	final class PluginBuilderProject(val info: ProjectInfo) extends BasicBuilderProject
	{
		override protected def logImpl = BuilderProject.this.log
		val pluginUptodate = propertyOptional[Boolean](false)
		def tpe = "plugin"
		def managedSourcePath = path("src_managed")
		override protected def definitionChanged() { setUptodate(false) }
		private def setUptodate(flag: Boolean)
		{
			pluginUptodate() = flag
			saveEnvironment()
		}
		
		private def pluginTask(f: => Option[String]) = task { if(!pluginUptodate.value) f else None }
		
		lazy val syncPlugins = pluginTask(sync()) dependsOn(extractSources)
		lazy val extractSources = pluginTask(extract()) dependsOn(update)
		lazy val update = pluginTask(loadAndUpdate()) dependsOn(compile)
		private def sync() = pluginCompileConditional.run orElse { setUptodate(true); None }
		private def extract() =
		{
			val sourcePluginJars = sourcePlugins.get
			FileUtilities.clean(managedSourcePath, log) orElse
			Control.lazyFold(sourcePluginJars.toList) { jar =>
				log.info("\tExtracting source plugin " + jar)
				FileUtilities.unzip(jar, extractTo(jar), log).left.toOption
			}
		}
		private def loadAndUpdate() =
		{
			Control.thread(projectDefinition) {
				case Some(definition) =>
					log.info("\nUpdating plugins")
					val pluginInfo = ProjectInfo(info.projectPath.asFile, Nil, None)
					val pluginBuilder = Project.constructProject(pluginInfo, Project.getProjectClass[PluginProject](definition, projectClasspath))
					pluginBuilder.projectName() = "Plugin builder"
					pluginBuilder.projectVersion() = OpaqueVersion("1.0")
					val result = pluginBuilder.update.run
					if(result.isEmpty)
					{
						log.success("Plugins updated successfully.")
						log.info("")
					}
					result
				case None => None
			}
		}
		def extractTo(jar: Path) =
		{
			val name = jar.asFile.getName
			managedSourcePath / name.substring(0, name.length - ".jar".length)
		}
		def sourcePlugins = descendents("source", jarFilter)
		def pluginClasspath = descendents("jar", jarFilter) +++ pluginCompileConfiguration.outputDirectory
		
		lazy val pluginCompileConditional = new BuilderCompileConditional(pluginCompileConfiguration)
		lazy val pluginCompileConfiguration =
			new BuilderCompileConfiguration
			{
				def label = "plugin builder"
				def sources = descendents(managedSourcePath, sourceFilter)
				def outputDirectory = outputPath / "plugin-classes"
				def classpath = sbtJarPath
				def analysisPath = outputPath / "plugin-analysis"
			}
	}
}
class PluginProject(val info: ProjectInfo) extends InternalProject with BasicManagedProject
{
	override final def outputPattern = "[type]/[artifact](-[revision]).[ext]"
	override final val tasks = Map("update" -> update)
	override def projectClasspath(config: Configuration) = Path.emptyPathFinder
	override def dependencies = info.dependencies
	override def managedDependencyPath = info.projectPath
}