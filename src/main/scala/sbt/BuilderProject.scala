/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

/** The project definition used to build project definitions. */
private final class BuilderProject(val info: ProjectInfo, override protected val logImpl: Logger) extends SimpleScalaProject
{
	import BasicProjectPaths._
	
	def compilePath = outputPath / DefaultMainCompileDirectoryName
	def mainSources = (info.projectPath * "*.scala") +++ path(DefaultSourceDirectoryName).descendentsExcept("*.scala", defaultExcludes)
	def mainResourcesPath = path(DefaultResourcesDirectoryName)
	def dependencyPath = path(DefaultDependencyDirectoryName)
	
	def libraries = descendents(dependencyPath, "*.jar")
	
	def compileOptions = Deprecation :: Unchecked :: Nil
	def javaCompileOptions = Nil
	override def tasks = Map.empty
	override def methods = Map.empty
	def dependencies = Nil
	
	override protected def disableCrossPaths = false
	
	def projectClasspath = compilePath +++ libraries +++
		Path.lazyPathFinder { Path.fromFile(FileUtilities.sbtJar) :: Nil }

	val compileConditional = new BuilderCompileConditional
	final class BuilderCompileConditional extends AbstractCompileConditional(compileConfiguration)
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
				val oldLevel = log.getLevel
				log.setLevel(Level.Info)
				log.info("Recompiling project definition...")
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
						log.debug("Found project definition " + subclassName)
						analysis.addProjectDefinition(sourcePath, subclassName)
					}
				}
			}
	}
	
	lazy val compile = task { compileConditional.run }
	lazy val clean = cleanTask(outputPath, ClearAnalysis(compileConditional.analysis))
	
	def compileConfiguration =
		new AbstractCompileConfiguration
		{
			def label = "builder"
			def sources = mainSources
			def outputDirectory = compilePath
			def classpath = projectClasspath
			def analysisPath = outputPath / DefaultMainAnalysisDirectoryName
			def projectPath = info.projectPath
			def log = BuilderProject.this.log
			def options = compileOptions.map(_.asString)
			def javaOptions = javaCompileOptions
			def maxErrors = ScalaProject.DefaultMaximumCompileErrors
			def compileOrder = CompileOrder.Mixed
		}
		
	def projectDefinition: Either[String, Option[String]] =
	{
		compileConditional.analysis.allProjects.toList match
		{
			case Nil => 
				log.debug("No project definitions detected using default project.")
				Right(None)
			case singleDefinition :: Nil => Right(Some(singleDefinition))
			case multipleDefinitions =>Left(multipleDefinitions.mkString("Multiple project definitions detected: \n\t","\n\t","\n"))
		}
	}
}