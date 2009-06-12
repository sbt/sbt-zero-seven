/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt.extract

import java.io.{ByteArrayOutputStream, File}
import FileUtilities.{classLocationFile, clean, createTemporaryDirectory, download, transferAndClose, unzip, write, zip}
import SelfExtractingProject.{flat, separator}

trait SelfExtractingProject extends Project
{
	protected def createSelfExtractingJar(actions: List[String], jvmOptions: List[String], projectZip: Path, outputJar: Path): Option[String] =
	{
		def jarForClass(name: String) = Path.fromFile(classLocationFile(Class.forName(name)))
		val loaderJar = jarForClass("sbt.boot.Boot")
		val bytes = new ByteArrayOutputStream
		transferAndClose(this.getClass.getResourceAsStream("extract.location"), bytes, log) orElse
		{
			val extractorJarLocation = bytes.toString("UTF-8")
			createSelfExtractingJar(actions, jvmOptions, projectZip, loaderJar, extractorJarLocation, outputJar)
		}
	}
	private def createSelfExtractingJar(actions: List[String], jvmOptions: List[String], projectZip: Path, loaderJar: Path, extractorJarLocation: String, outputJar: Path): Option[String] =
	{
		val installContents = jvmOptions.mkString("\n") + separator + actions.mkString("\n")
		withTemporaryDirectory(log) { tmp =>
			val tmpPath = Path.fromFile(tmp)
			write(new File(tmp, "install"), installContents, log) orElse
			unzip(this.getClass.getResource(extractorJarLocation), tmpPath, log).left.toOption orElse
			zip( (tmpPath ##) :: flat(projectZip) :: loaderJar :: Nil, outputJar, true, log)
		}
	}
	private def withTemporaryDirectory(log: Logger)(f: File => Option[String]) =
	{
		Control.thread(createTemporaryDirectory(log)) { dir =>
			Control.trapUnitAndFinally("", log)
				{ f(dir) }
				{ clean(Path.fromFile(dir) :: Nil, true, log) }
		}
	}
}
trait BasicSelfExtractingProject extends BasicScalaProject with SelfExtractingProject
{
	def installActions: List[String] = update.name :: `package`.name :: Nil
	def jvmOptions: List[String] = Nil
	def selfExtractingJar: Path = outputPath / (artifactBaseName + "-setup.jar")
	
	lazy val installer = installerAction
	def installerAction = task { createSelfExtractingJar(installActions, jvmOptions, packageProjectZip, selfExtractingJar) } dependsOn packageProject
}

object SelfExtractingProject
{
	// keep this in sync with sbt.extract.Main.separator
	def separator = "===================="
	private def flat(p: Path) =
		p match
		{
			case rp: RelativePath => (rp.parentPath ##) / rp.component
			case _ => p
		}
}