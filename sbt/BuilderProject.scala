/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

final class BuilderProject(val info: ProjectInfo) extends ScalaProject with ConsoleLogger
{
	setLevel(Level.Warn)
	
	import BasicProjectPaths._
	
	def outputPath = path(DefaultOutputDirectoryName)
	def compilePath = outputPath / DefaultCompileDirectoryName
	def sourcePath = path(DefaultSourceDirectoryName)
	def mainResourcesPath = path(DefaultResourcesDirectoryName)
	def dependencyPath = path(DefaultDependencyDirectoryName)
	
	def defaultExcludes = ".svn" | ".cvs"
	def defaultIncludeAll = -defaultExcludes
	def libraries = dependencyPath ** defaultIncludeAll * "*.jar"
	
	def compileOptions = Deprecation :: Unchecked :: Nil
	override def tasks = Map.empty
	def dependencies = Nil
	
	
	def projectClasspath = compilePath +++ libraries +++
		Path.lazyPathFinder { new ProjectDirectory(FileUtilities.sbtJar) :: Nil }

	val compileConditional =
		new CompileConditional(compileConfiguration)
		{
			override def analysisCallback: AnalysisCallback =
				new BasicAnalysisCallback(info.projectPath, List(Project.ProjectClassName), analysis)
				{
					def foundSubclass(sourcePath: Path, subclassName: String, superclassName: String, isModule: Boolean)
					{
						if(superclassName == Project.ProjectClassName && !isModule)
						{
							debug("Found project definition " + subclassName)
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
			def sources = sourcePath ** defaultIncludeAll * "*.scala"
			def outputDirectory = compilePath
			def classpath = projectClasspath
			def analysisPath = outputPath / DefaultAnalysisDirectoryName
			def projectPath = info.projectPath
			def testDefinitionClassNames = Nil
			def log = BuilderProject.this
			def options = compileOptions.map(_.asString)
		}
		
	def projectDefinition: Option[String] =
	{
		compileConditional.analysis.allProjects.toList match
		{
			case Nil => 
				debug("No project definitions detected: expecting explicit configuration.")
				None
			case singleDefinition :: Nil => Some(singleDefinition)
			case _ =>
				debug("Multiple project definitions detected: expecting explicit configuration.")
				None
		}
	}
	
}