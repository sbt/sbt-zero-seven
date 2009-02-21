/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.File
import FileUtilities._

final case class ProjectInfo(projectDirectory: File, dependencies: Iterable[Project], parent: Option[Project]) extends NotNull
{
	val projectPath = new ProjectDirectory(projectDirectory)
	val builderPath = projectPath / ProjectInfo.MetadataDirectoryName
}

private[sbt] sealed trait SetupResult extends NotNull
private[sbt] final object SetupDeclined extends SetupResult
private[sbt] final class SetupError(val message: String) extends SetupResult
private[sbt] final object AlreadySetup extends SetupResult
private[sbt] final class SetupInfo(val name: String, val version: Option[Version], val initializeDirectories: Boolean) extends SetupResult

object ProjectInfo
{
	val MetadataDirectoryName = "project"
	
	def setup(info: ProjectInfo, log: Logger): SetupResult =
	{
		val builderDirectory = info.builderPath.asFile
		if(builderDirectory.exists)
		{
			if(builderDirectory.isDirectory)
				AlreadySetup
			else
				new SetupError("'" + builderDirectory.getAbsolutePath + "' is not a directory.")
		}
		else
			setupProject(info.projectDirectory, log)
	}
	private def setupProject(projectDirectory: File, log: Logger): SetupResult =
	{
		if(confirmPrompt("No project found. Create new project?", false))
		{
			val name = trim(Console.readLine("Project Name: "))
			if(name.isEmpty)
				new SetupError("Project not created: no name specified.")
			else
				readVersion(projectDirectory, log) match
				{
					case None => new SetupError("Project not created: no version specified.")
					case Some(version) =>
						if(verifyCreateProject(name, version))
							new SetupInfo(name, Some(version), true)
						else
							SetupDeclined
				}
		}
		else
			SetupDeclined
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