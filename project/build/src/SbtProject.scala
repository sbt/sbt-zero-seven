import sbt._

class SbtProject(info: ProjectInfo) extends DefaultProject(info)
{
	override def defaultJarBaseName = "sbt-" + version.toString
	
	
	println("Main classes: " + ((mainCompilePath) *** "*.class").get)
}