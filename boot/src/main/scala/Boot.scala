/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
 package sbt

import java.io.{File, FileFilter}
import java.net.{URL, URLClassLoader}

import BootConfiguration._
import UpdateTarget.{UpdateScala, UpdateSbt}

private class BootException(override val toString: String) extends RuntimeException
object Boot
{
	def main(args: Array[String])
	{
		try { boot(args) }
		catch
		{
			case b: BootException => errorAndExit(b)
			case e =>
				e.printStackTrace
				errorAndExit(e)
		}
	}
	private def errorAndExit(e: Throwable)
	{
		System.out.println("Error during sbt execution: " + e.toString)
		System.exit(1)
	}
	private def boot(args: Array[String])
	{
		(new Setup).checkProject()
		if(args.length == 0)
			load(args)
		else
			runBatch(args.toList, Nil)
	}
	private def runBatch(args: List[String], accumulateReversed: List[String])
	{
		def doLoad() = if(!accumulateReversed.isEmpty) load(accumulateReversed.reverse.toArray)
		args match
		{
			case Nil => doLoad()
			case RebootCommand :: tail =>
				doLoad()
				runBatch(tail, Nil)
			case notReload :: tail => runBatch(tail, notReload :: accumulateReversed)
		}
	}
	private def load(args: Array[String])
	{
		val classpath = (new Setup).classpath()
		val loader = new URLClassLoader(classpath, new FilteredLoader)
		val sbtMain = Class.forName(SbtMainClass, true, loader)
		val mainMethod = sbtMain.getMethod(MainMethodName, classOf[Array[String]])
		mainMethod.invoke(null, Array(args) : _*)
	}
}

private class Setup extends NotNull
{
	import Setup._
	private val ProjectDirectory = new File(ProjectDirectoryName)
	private val BootDirectory = new File(ProjectDirectory, BootDirectoryName)
	private val PropertiesFile = new File(ProjectDirectory, BuildPropertiesName)
	
	final def checkProject()
	{
		if(!ProjectDirectory.exists)
		{
			val line = Console.readLine("Project does not exist, create new project? (y/N) : ")
			if(isYes(line))
				ProjectProperties(PropertiesFile, true)
			else
				System.exit(1)
		}
	}
	
	final def classpath(): Array[URL] =
	{
		val (scalaVersion, sbtVersion) = ProjectProperties(PropertiesFile, false)
		updateVersions(scalaVersion, sbtVersion).toArray
	}
	private def updateVersions(scalaVersion: String, sbtVersion: String) =
	{
		val baseDirectory = new File(BootDirectory, baseDirectoryName(scalaVersion))
		System.setProperty(ScalaHomeProperty, baseDirectory.getAbsolutePath)
		val scalaDirectory = new File(baseDirectory, ScalaDirectoryName)
		val sbtDirectory = new File(baseDirectory, sbtDirectoryName(sbtVersion))
		
		lazy val update = new Update(baseDirectory, sbtVersion, scalaVersion)
		if(!scalaDirectory.exists)
		{
			update.update(UpdateScala)
			if(!scalaDirectory.exists)
				retrieveError("Scala", scalaDirectory)
		}
		if(!sbtDirectory.exists)
		{
			update.update(UpdateSbt)
			if(!sbtDirectory.exists)
				retrieveError("Sbt", sbtDirectory)
		}
		getJars(scalaDirectory, sbtDirectory)
	}
}
private object Setup
{
	private def isYes(s: String) =
		s != null &&
		{
			val trimmed = s.trim.toLowerCase
			trimmed == "y" || trimmed == "yes"
		}
	private def retrieveError(name: String, directory: File) =
		throw new BootException(name + " was not properly retrieved (expected directory " + directory + " to exist).")
	private def getJars(directories: File*) = directories.flatMap(file => wrapNull(file.listFiles(JarFilter))).map(_.toURI.toURL)
	private def wrapNull(a: Array[File]): Array[File] = if(a == null) Array() else a
}

private object JarFilter extends FileFilter
{
	def accept(file: File) = !file.isDirectory && file.getName.endsWith(".jar")
}
