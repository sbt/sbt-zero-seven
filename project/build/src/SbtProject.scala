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
	override def ivyXML =
		(<configurations>
			<conf name="base"/>
			<conf name="2.7.2" extends="base"/>
			<conf name="2.7.3" extends="base"/>
			<conf name="optional-base"/>
			<conf name="optional-2.7.2" extends="optional-base"/>
			<conf name="optional-2.7.3" extends="optional-base"/>
			<conf name="compile" extends="2.7.2" visibility="private"/>
			<conf name="scalac-2.7.2" visibility="private"/>
			<conf name="scalac-2.7.3" visibility="private"/>
		</configurations>
		<publications>
			<artifact name="sbt_2.7.2" conf="2.7.2"/>
			<artifact name="sbt_2.7.3" conf="2.7.3"/>
		</publications>
		<dependencies>
			<dependency org="org.apache.ivy" name="ivy" rev="2.0.0" transitive="false" conf="base->default"/>
			<dependency org="org.scalacheck" name="scalacheck" rev="1.5" transitive="false" conf="optional-base->default"/>
			
			<dependency org="org.specs" name="specs" rev="1.4.0" transitive="false" conf="optional-2.7.2->default"/>
			<dependency org="org.scalatest" name="scalatest" rev="0.9.3" transitive="false" conf="optional-2.7.2->default"/>
			<dependency org="org.scala-lang" name="scala-compiler" rev="2.7.2" conf="scalac-2.7.2->default"/>
			
			<dependency org="org.scala-tools.testing" name="scalatest" rev="0.9.4" transitive="false" conf="optional-2.7.3->default"/>
			<dependency org="org.specs" name="specs" rev="1.4.3" transitive="false" conf="optional-2.7.3->default"/>
			<dependency org="org.scala-lang" name="scala-compiler" rev="2.7.3" conf="scalac-2.7.3->default"/>
		</dependencies>)
	
	private val conf_2_7_2 = config("2.7.2")
	private val conf_2_7_3 = config("2.7.3")
	private val allConfigurations = conf_2_7_2 :: conf_2_7_3 :: Nil
	
	def optionalClasspath(version: String) = fullClasspath(config("optional-" + version)) +++ super.optionalClasspath
	
	private val CompilerMainClass = "scala.tools.nsc.Main"
	private val conf = new DefaultPublishConfiguration("local", "release")
	{
		override def configurations: Option[Iterable[Configuration]] = Some(config("base") :: allConfigurations)
	}
	lazy val crossPackage = allConfigurations.map(conf => packageForScala(conf.toString))
	lazy val crossDeliverLocal = deliverTask(conf, updateOptions) dependsOn(crossPackage : _*)
	lazy val crossPublishLocal = publishTask(conf, updateOptions) dependsOn(crossDeliverLocal)
	private def packageForScala(scalaVersion: String) =
	{
		val classes = classesPath(scalaVersion) ** "*"
		val jarName = "sbt_" + scalaVersion + "-" + version.toString +  ".jar"
		FileUtilities.clean((outputPath / jarName) :: Nil, log) // TODO: temporary, remove when >0.3.8 released
		packageTask(classes +++ mainResources, outputPath, jarName, packageOptions).dependsOn(compileForScala(scalaVersion))
	}
	private def compileForScala(version: String)=
		task
		{
			val classes = classesPath(version)
			FileUtilities.createDirectory(classes, log)
			val compilerClasspath = concatPaths(fullClasspath(config("scalac-" + version)))
			
			val classpath = fullClasspath(config(version)) +++ optionalClasspath(version)
			val sources: List[String] = pathListStrings(mainSources.get)
			val compilerArguments: List[String] = List("-cp", concatPaths(classpath), "-d", classes.toString) ::: sources
			
			val allArguments = "-Xmx256M" :: ("-Xbootclasspath/a:" + compilerClasspath) :: CompilerMainClass :: compilerArguments
			val process = (new ProcessRunner("java", allArguments)).mergeErrorStream.logIO(log)
			val exitValue = process.run.exitValue
			if(exitValue == 0)
				None
			else
				Some("Nonzero exit value (" + exitValue + ") when calling scalac with arguments: \n" +
					compilerArguments.mkString(" ") + "\nand classpath: \n" + 
					compilerClasspath)
		}
	private def concatPaths(p: PathFinder): String = pathListStrings(p.get).mkString(File.pathSeparator)
	private def pathListStrings(p: Iterable[Path]): List[String] = p.map(_.asFile.getAbsolutePath).toList
	private def classesPath(scalaVersion: String) = "target"  / ("classes-" + scalaVersion) ##
	override def parallelExecution = true
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