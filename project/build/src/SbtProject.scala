/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
import sbt._

import java.io.File

class SbtProject(info: ProjectInfo) extends ParentProject(info)
{
	// Launcher sub project.
	lazy val boot = project("boot", "Simple Build Tool Loader", new LoaderProject(_))
	// Main builder sub project
	lazy val main = project(info.projectPath, "Simple Build Tool Builder", new MainProject(_))
	
	// One-shot build for users building from trunk
	lazy val fullBuild = task { None } dependsOn(boot.proguard, main.crossPublishLocal) describedAs
		"Builds the loader and builds main sbt against all supported versions of Scala and installs to the local repository."
		
	override def shouldCheckOutputDirectories = false
	override def baseUpdateOptions = QuietUpdate :: Nil
}

protected class MainProject(val info: ProjectInfo) extends CrossCompileProject
{
	override def defaultJarBaseName = "sbt_" + version.toString
	/** Additional resources to include in the produced jar.*/
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	override def mainClass = Some("sbt.Main")
	override def testOptions = ExcludeTests("sbt.ReflectiveSpecification" :: Nil) :: super.testOptions.toList
	
	// ======== Scripted testing ==========
	
	def sbtTestResources = testResourcesPath / "sbt-test-resources"
	
	lazy val testNoScripted = super.testAction
	override def testAction = testNoScripted dependsOn(scripted)
	lazy val scripted = scriptedTask dependsOn testCompile
	def scriptedTask =
		task
		{
			log.info("Running scripted tests...")
			log.info("")
			// load ScriptedTests using a ClassLoader that loads from the project classpath so that the version
			// of sbt being built is tested, not the one doing the building.
			val loader = ScriptedLoader(runClasspath.get.map(_.asURL).toSeq.toArray)
			val scriptedClass = Class.forName(ScriptedClassName, true, loader).asSubclass(classOf[Scripted])
			val scriptedConstructor = scriptedClass.getConstructor(classOf[File], classOf[ScriptedTestFilter])
			val runner = scriptedConstructor.newInstance(sbtTestResources.asFile, filter)
			runner.scriptedTests(log)
		}
	val ScriptedClassName = "ScriptedTests"
	
	val filter = new ScriptedTestFilter
	{
		def accept(group: String, name: String) = name == "multi"
	}
}