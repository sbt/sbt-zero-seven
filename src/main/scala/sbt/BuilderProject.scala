/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

/** The project definition used to build project definitions. */
final class BuilderProject(val info: ProjectInfo) extends ScalaProject
{
	override protected def logImpl = new ConsoleLogger
	log.setLevel(Level.Warn)
	
	import BasicProjectPaths._
	
	def outputPath = path(DefaultOutputDirectoryName)
	def compilePath = outputPath / DefaultMainCompileDirectoryName
	def sourcePath = path(DefaultSourceDirectoryName)
	def mainResourcesPath = path(DefaultResourcesDirectoryName)
	def dependencyPath = path(DefaultDependencyDirectoryName)
	
	def libraries = descendents(dependencyPath, "*.jar")
	
	def compileOptions = Deprecation :: Unchecked :: Nil
	override def tasks = Map.empty
	def dependencies = Nil
	
	def projectClasspath = compilePath +++ libraries +++
		Path.lazyPathFinder { new ProjectDirectory(FileUtilities.sbtJar) :: Nil }

	val compileConditional =
		new CompileConditional(compileConfiguration)
		{
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
			override def analysisCallback: AnalysisCallback =
				new BasicAnalysisCallback(info.projectPath, List(Project.ProjectClassName), analysis)
				{
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
		new CompileConfiguration
		{
			def label = "builder"
			def sources = sourcePath.descendentsExcept("*.scala", defaultExcludes)
			def outputDirectory = compilePath
			def classpath = projectClasspath
			def analysisPath = outputPath / DefaultMainAnalysisDirectoryName
			def projectPath = info.projectPath
			def testDefinitionClassNames = Nil
			def log = BuilderProject.this.log
			def options = compileOptions.map(_.asString)
		}
		
	def projectDefinition: Option[String] =
	{
		compileConditional.analysis.allProjects.toList match
		{
			case Nil => 
				log.debug("No project definitions detected: expecting explicit configuration.")
				None
			case singleDefinition :: Nil => Some(singleDefinition)
			case _ =>
				log.debug("Multiple project definitions detected: expecting explicit configuration.")
				None
		}
	}
}