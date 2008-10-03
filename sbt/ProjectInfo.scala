package sbt

import java.io.File
import FileUtilities._

final case class ProjectInfo(name: String, currentVersion: Version, builderClassName: String, projectDirectory: File)
	(private[sbt] val id: Int) extends NotNull
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
	{
		import java.io.{BufferedWriter, FileWriter}
		
		val writerE: Either[String, BufferedWriter] =
			try { Right(new BufferedWriter(new FileWriter(new File(info.builderPath.asFile, ProjectInfoFileName)))) }
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
		}).left.toOption
	}
	def load(projectDirectory: File, id: Int, log: Logger): Either[String, ProjectInfo] =
	{
		val builderDirectory = new File(projectDirectory, MetadataDirectoryName)
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
					loadProjectInfo(projectDirectory, builderDirectory, projectInfoFile, id, log)
			}
		}
	}
	private def loadProjectInfo(workingDirectory: File, builderDirectory: File, projectInfoFile: File, id: Int, log: Logger) =
	{
		import scala.io.Source
		val linesE: Either[String, List[String]] =
			try
			{
				Right(Source.fromFile(projectInfoFile).getLines.toList.map(_.trim).filter(line =>
					!line.isEmpty && line.charAt(0) != '#'))
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
					ProjectInfo(name, version, builderClassName, workingDirectory)(id)
			}
			else
				Left("Project information file invalid: expected name, version, and (optionally) builder class.")
		})
	}
}