/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
import sbt._

import java.net.URL
import java.io.File

class SbtProject(info: ProjectInfo) extends DefaultProject(info)
{
	override def defaultJarBaseName = "sbt-" + version.toString
	/** Additional resources to include in the produced jar.*/
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	override def mainClass = Some("sbt.Main")
	override def testOptions = ExcludeTests("sbt.ReflectiveSpecification" :: Nil) :: super.testOptions.toList
	
	def sbtTestResources = testResourcesPath / "sbt-test-resources"
	/*
	override def testAction = super.testAction dependsOn(scripted)
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
		def accept(group: String, name: String) = true
			//group == "tests" && name == "specs-nested"
	}
	//override protected def includeTest(test: String): Boolean = true
		//test == "sbt.WriteContentSpecification"
*/
}
package sbt { // need access to LoaderBase, which is private in package sbt
	object ScriptedLoader
	{
		def apply(paths: Array[URL]): ClassLoader = new ScriptedLoader(paths)
	}
	private class ScriptedLoader(paths: Array[URL]) extends LoaderBase(paths, ScriptedLoader.getClass.getClassLoader)
	{
		def doLoadClass(className: String): Class[_] =
		{
			// Logger needs to be loaded from the version of sbt building the project because we need to pass
			// a Logger from that loader into ScriptedTests.
			// All other sbt classes should be loaded from the project classpath so that we test those classes with 'scripted'
			if(className != "sbt.Logger" && className.startsWith("sbt."))
				findClass(className)
			else
				selfLoadClass(className)
		}
	}
}