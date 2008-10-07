package sbt

class BuilderProject(val info: ProjectInfo, val analysis: ProjectAnalysis) extends Project with ConsoleLogger
{
	setLevel(Level.Warn)
	
	// flatten the source file hierarchy a bit
	override def mainSourcePath = sourcePath
	override def mainScalaSourcePath = sourcePath
	override def mainResourcesPath = path(resourcesDirectoryName)
	
	def actions = Map.empty
	
	def mainSources = sourcePath ** "*.scala" - ".svn"
	def compileOptions = Deprecation :: Nil

  val compile = compileTask(mainSources, compilePath, compileOptions, true)
  val clean = cleanTask(outputPath, ClearAnalysis :: Nil)
}
