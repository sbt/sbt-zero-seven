/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
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
	
	/*def sbtTestResources = testResourcesPath / "sbt-test-resources"
	
	override def testAction = super.testAction dependsOn(scripted)
	lazy val scripted =
		task
		{
			log.info("Running scripted tests...")
			log.info("")
			(new ScriptedTests(new Resources(sbtTestResources.asFile), filter)).scriptedTests(log)
		}
		
	val filter = new TestFilter
	{
		def accept(group: String, name: String) = true
			//group == "package" && name == "manifest"
	}*/
}