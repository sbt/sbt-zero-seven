/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
 package sbt

// This is the main class for the sbt launcher.  Its purpose is to ensure the appropriate
// versions of sbt and scala are downloaded to the projects 'project/boot' directory.
// Then, the downloaded version of sbt is started as usual using the right version of
// scala.

// Artifact names must be consistent between the main sbt build and this build.

import java.io.{File, FileFilter}
import java.net.{URL, URLClassLoader}

// contains constants and paths
import BootConfiguration._
import UpdateTarget.{UpdateScala, UpdateSbt}

// The exception to use when an error occurs at the launcher level (and not a nested exception).
// This indicates overrides toString because the exception class name is not needed to understand
// the error message.
private class BootException(override val toString: String) extends RuntimeException
// The entry point to the launcher
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
		 // prompt to create project if it doesn't exist.
		 // will not return if user declines
		(new Setup).checkProject()
		if(args.length == 0)
			load(args) // interactive mode, which can only use one version of scala for a run
		else
			runBatch(args.toList, Nil)  // batch mode, which can reboot with a different scala version
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
	/** Loads the project in the current working directory using the version of scala and sbt
	* declared in the build. The class loader used prevents the Scala and Ivy classes used by
	* this loader from being seen by the loaded sbt/project.*/
	private def load(args: Array[String])
	{
		val classpath = (new Setup).classpath()
		val loader = new URLClassLoader(classpath, new FilteredLoader)
		val sbtMain = Class.forName(SbtMainClass, true, loader)
		val mainMethod = sbtMain.getMethod(MainMethodName, classOf[Array[String]])
		mainMethod.invoke(null, Array(args) : _*)
	}
}

/** A class to handle setting up the properties and classpath of the project
* before it is loaded. */
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
	/** Checks that the requested version of sbt and scala have been downloaded.
	* It performs a simple check that the appropriate directories exist.  It does
	* not actually verify that appropriate classes are resolvable.  It uses Ivy
	* to resolve and retrieve any necessary libraries. The classpath to use is returned.*/
	private def updateVersions(scalaVersion: String, sbtVersion: String) =
	{
		val baseDirectory = new File(BootDirectory, baseDirectoryName(scalaVersion))
		System.setProperty(ScalaHomeProperty, baseDirectory.getAbsolutePath)
		val scalaDirectory = new File(baseDirectory, ScalaDirectoryName)
		val sbtDirectory = new File(baseDirectory, sbtDirectoryName(sbtVersion))
		
		val targets = checkTarget(scalaDirectory, UpdateScala) ::: checkTarget(sbtDirectory, UpdateSbt)
		Update(baseDirectory, sbtVersion, scalaVersion, targets: _*)
		verifyUpdated("Scala", scalaDirectory)
		verifyUpdated("Sbt", sbtDirectory)
		getJars(scalaDirectory, sbtDirectory)
	}
}
private object Setup
{
	private def verifyUpdated(label: String, dir: File)
	{
		if(!dir.exists)
			retrieveError(label, dir)
	}
	private def checkTarget(dir: File, target: UpdateTarget.Value) = if(!dir.exists) target :: Nil else Nil
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
