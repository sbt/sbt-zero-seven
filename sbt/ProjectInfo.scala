/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.File
import FileUtilities._

final case class ProjectInfo(name: String, currentVersion: Version, builderClassName: String, projectDirectory: File)
	(private[sbt] val initializeDirectories: Boolean) extends NotNull
{
	import ProjectInfo._
	val projectPath = new ProjectDirectory(projectDirectory)
	val builderPath = projectPath / MetadataDirectoryName
	val infoPath = builderPath / ProjectInfoFileName
}

object ProjectInfo
{
	val DefaultBuilderClassName = "sbt.DefaultProject"
	val MetadataDirectoryName = "metadata"
	val ProjectInfoFileName = "info"
	
	def write(info: ProjectInfo, log: Logger): Option[String] =
		FileUtilities.createDirectory(info.builderPath.asFile, log) orElse doWrite(info, log)
	private def doWrite(info: ProjectInfo, log: Logger): Option[String] =
	{
		import java.io.{BufferedWriter, FileWriter}
		val writerE: Either[String, BufferedWriter] =
			try
			{
				Right(new BufferedWriter(new FileWriter(new File(info.builderPath.asFile, ProjectInfoFileName))))
			}
			catch { case e: Exception => log.trace(e); Left(e.getMessage) }
		
		writerE.right.flatMap( writer =>
		{
			try
			{
				writer.write(info.name)
				writer.newLine()
				writer.write(info.currentVersion.toString)
				writer.newLine()
				if(DefaultBuilderClassName != info.builderClassName)
				{
					writer.write(info.builderClassName)
					writer.newLine()
				}
				Right(())
			}
			catch
			{
				case e: Exception => log.trace(e); Left(e.getMessage)
			}
			finally { writer.close }
		}).left.toOption
	}
	def load(projectDirectory: File, log: Logger): Either[String, ProjectInfo] =
	{
		val builderDirectory = new File(projectDirectory, MetadataDirectoryName)
		checkBuilderDirectory(projectDirectory, builderDirectory, log).right.flatMap
		{
			(initializeDirectories: Boolean) =>
			{
				if(!builderDirectory.isDirectory)
					Left("'" + printableFilename(builderDirectory) + "' is not a directory.")
				else
				{
					val projectInfoFile = new File(builderDirectory, ProjectInfoFileName)
					if(!projectInfoFile.exists)
						Left("Project information file '" + printableFilename(projectInfoFile) + "' does not exist.")
					else
					{
						if(projectInfoFile.isDirectory)
							Left("Project information file '" + printableFilename(projectInfoFile) + "' is a directory.")
						else
							loadProjectInfo(projectDirectory, builderDirectory, projectInfoFile, initializeDirectories, log)
					}
				}
			}
		}
	}
	private def checkBuilderDirectory(projectDirectory: File, builderDirectory: File, log: Logger):
		Either[String, Boolean] =
	{
		if(builderDirectory.exists)
			Right(false)
		else
		{
			if(setupProject(projectDirectory, log))
				Right(true)
			else
				Left("No project found.")
		}
	}
	private def loadProjectInfo(workingDirectory: File, builderDirectory: File,
		projectInfoFile: File, initialize: Boolean, log: Logger) =
	{
		import scala.io.Source
		val linesE: Either[String, List[String]] =
			try
			{
				Right(Source.fromFile(projectInfoFile).getLines.toList.map(_.trim).filter(line =>
					line.length > 0 && line.charAt(0) != '#'))
			}
			catch
			{
				case e: Exception =>
				{
					log.trace(e)
					Left("Error reading project information from '" + printableFilename(projectInfoFile) + "': " + e.getMessage)
				}
			}
		linesE.right flatMap (lines =>
		{
			val length = lines.length
			if(2 <= length && length <= 3)
			{
				val name :: versionString :: tail = lines
				val builderClassName = tail.firstOption.getOrElse(DefaultBuilderClassName)
				for(version <- Version.fromString(versionString).right) yield
					ProjectInfo(name, version, builderClassName, workingDirectory)(initialize)
			}
			else
				Left("Project information file invalid: expected name, version, and (optionally) builder class.")
		})
	}
	private def setupProject(projectDirectory: File, log: Logger): Boolean =
	{
		if(confirmPrompt("No project found. Create new project?", false))
		{
			val name = trim(Console.readLine("Project Name: "))
			if(name.isEmpty)
				false
			else
			{
				readVersion(projectDirectory, log) match
				{
					case Some(version) =>
						if(verifyCreateProject(name, version))
							createProject(name, version, projectDirectory, log)
						else
							setupProject(projectDirectory, log)
					case None => false
				}
			}
		}
		else
			false
	}
	private def verifyCreateProject(name: String, version: Version): Boolean =
		confirmPrompt("Create new project " + name + " " + version + " ?", true)
	
	private def confirmPrompt(question: String, defaultYes: Boolean) =
	{
		val choices = if(defaultYes) " (Y/n) " else " (y/N) "
		val answer = trim(Console.readLine(question + choices))
		val yes = "y" :: "yes" :: (if(defaultYes) List("") else Nil)
		yes.contains(answer.toLowerCase)
	}
	
	private def createProject(name: String, version: Version, projectDirectory: File, log: Logger): Boolean =
	{
		val tempInfo = ProjectInfo(name, version, DefaultBuilderClassName, projectDirectory)(false)
		write(tempInfo, log) match
		{
			case Some(errorMessage) =>
			{
				log.error("Could not write project info: " + errorMessage)
				false
			}
			case None =>
			{
				log.info("Wrote project information to " + tempInfo.infoPath.relativePath)
				true
			}
		}
	}
	
	private def readVersion(projectDirectory: File, log: Logger): Option[Version] =
	{
		val version = trim(Console.readLine("Version: "))
		if(version.isEmpty)
			None
		else
		{
			Version.fromString(version) match
			{
				case Left(errorMessage) =>
				{
					log.error("Invalid version: " + errorMessage)
					readVersion(projectDirectory, log)
				}
				case Right(v) => Some(v)
			}
		}
	}
	private def trim(s: String) = if(s == null) "" else s.trim
}