import sbt._

class SbtProject(info: ProjectInfo) extends DefaultProject(info)
{
	override def defaultJarBaseName = "sbt-" + version.toString
	/** Additional resources to include in the produced jar.*/
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	/** This specifies the Main-Class attribute in the manifest.*/
	override def mainClass = Some("sbt.Main")
	override def testOptions = ExcludeTests("sbt.ReflectiveSpecification" :: Nil) :: super.testOptions.toList
}