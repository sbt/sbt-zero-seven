/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

final class BuilderProject(val info: ProjectInfo) extends ScalaProject with ConsoleLogger
{
	setLevel(Level.Warn)
	
	// flatten the source file hierarchy a bit
	override def mainSourcePath = sourcePath
	override def mainScalaSourcePath = sourcePath
	override def mainResourcesPath = path(resourcesDirectoryName)
	
	def projectDefinition: Option[String] =
	{
		analysis.allProjects.toList match
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
	
	def mainSources = sourcePath ** "*.scala" - ".svn"
	def compileOptions = Deprecation :: Nil
	override def tasks = Map.empty
	def dependencies = Nil

	lazy val compile = compileTask(mainSources, compilePath, compileOptions, true)
	lazy val clean = cleanTask(outputPath, ClearAnalysis :: Nil)
}