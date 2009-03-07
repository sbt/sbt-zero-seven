/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
package sbt

import java.io.File
import FileUtilities._

object Resources
{
	def apply(basePath: String) =
	{
		require(basePath.startsWith("/"))
		val resource = getClass.getResource(basePath)
		if(resource == null)
			throw new Exception("Resource base directory '" + basePath + "' not on classpath.")
		else
		{
			val file = new File(resource.toURI)
			if(file.exists)
				new Resources(file)
			else
				throw new Exception("Resource base directory '" + basePath + "' does not exist.")
		}
	}
	private val LoadErrorPrefix = "Error loading initial project: "
}

class Resources(val baseDirectory: File)
{
	import Resources._
	// The returned directory is not actually read-only, but it should be treated that way
	def readOnlyResourceDirectory(group: String, name: String): Either[String, File] =
	{
		val groupDirectory = new File(baseDirectory, group)
		if(groupDirectory.isDirectory)
		{
			val resourceDirectory = new File(groupDirectory, name)
			if(resourceDirectory.isDirectory)
				Right(resourceDirectory)
			else
				Left("Resource directory '" + name + "' in group '" + group + "' not found.")
		}
		else
			Left("Group '" + group + "' not found.")
	}
	def readWriteResourceDirectory[T](group: String, name: String, log: Logger)
		(withDirectory: File => Either[String, T]): Either[String, T] =
			readOnlyResourceDirectory(group, name).right flatMap(file => readWriteResourceDirectory(file, log)(withDirectory))
	def readWriteResourceDirectory[T](readOnly: File, log: Logger)
		(withDirectory: File => Either[String, T]): Either[String, T] =
	{
		require(readOnly.isDirectory)
		def readWrite(readOnly: File)(temporary: File): Either[String, T] =
		{
			val readWriteDirectory = new File(temporary, readOnly.getName)
			FileUtilities.copyDirectory(readOnly, readWriteDirectory, log).toLeft(()).right flatMap { x => 
				withDirectory(readWriteDirectory)
			}
		}
		doInTemporaryDirectory(log)(readWrite(readOnly))
	}
	
	def withProject[T](projectDirectory: File, log: Logger)(f: Project => WithProjectResult[T]): Either[String, T] =
		readWriteResourceDirectory(projectDirectory, log)(withProject(log)(f))
	def withProject[T](group: String, name: String, log: Logger)(f: Project => WithProjectResult[T]): Either[String, T] =
		readWriteResourceDirectory(group, name, log)(withProject(log)(f))
	def withProject[T](log: Logger)(f: Project => WithProjectResult[T])(dir: File): Either[String, T] =
		withProject(log, Left(LoadErrorPrefix), dir )(f)
	private def withProject[T](log: Logger, useProject: Either[String,Project], dir: File)(f: Project => WithProjectResult[T]): Either[String, T] =
	{
		val loadResult =
			useProject.left.flatMap { reloadPrefixIfError =>
				val buffered = new BufferedLogger(log)
				buffered.startRecording()
				resultToEither(Project.loadProject(dir, Nil, None, buffered)) match
				{
					case Left(msg) =>
						buffered.play()
						buffered.clear()
						Left(reloadPrefixIfError + msg)
					case Right(p) => 
						buffered.clear()
						Right(p)
				}
			}
		loadResult match
		{
			case Right(project) =>
				f(project) match
				{
					case ContinueResult(newF, reloadAndPrefixIfError) => withProject(log, reloadAndPrefixIfError.toLeft(project), dir)(newF)
					case ValueResult(value) => Right(value)
					case err: ErrorResult => Left(err.message)
				}
			case Left(message) => Left(message)
		}
	}

	def resultToEither(result: LoadResult): Either[String, Project] =
		result match
		{
			case success: LoadSuccess => Right(success.project)
			case err: LoadError => Left(err.message)
			case err: LoadSetupError => Left(err.message)
			case LoadSetupDeclined => Left("Setup declined")
		}
}
sealed trait WithProjectResult[+T] extends NotNull
final case class ContinueResult[T](f: Project => WithProjectResult[T], reloadAndPrefixIfError: Option[String]) extends WithProjectResult[T]
final case class ValueResult[T](value: T) extends WithProjectResult[T]
final class ErrorResult(val message: String) extends WithProjectResult[Nothing]