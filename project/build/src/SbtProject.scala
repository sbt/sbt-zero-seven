import sbt._

class SbtProject(info: ProjectInfo) extends DefaultProject(info)
{
	override def defaultJarBaseName = "sbt-" + version.toString
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	override def mainClass = Some("sbt.Main")
}