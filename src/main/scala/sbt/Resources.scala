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
}

class Resources(val baseDirectory: File)
{
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
	
	def withProject[T](projectDirectory: File, log: Logger)(f: Project => Either[String, T]): Either[String, T] =
		readWriteResourceDirectory(projectDirectory, log)
			{ dir => resultToEither(Project.loadProject(dir, Nil, None)).right.flatMap(f) }
	def withProject[T](group: String, name: String, log: Logger)(f: Project => Either[String, T]): Either[String, T] =
		readWriteResourceDirectory(group, name, log)
			{ dir => resultToEither(Project.loadProject(dir, Nil, None)).right.flatMap(f) }

	def resultToEither(result: LoadResult): Either[String, Project] =
		result match
		{
			case LoadSuccess(project) => Right(project)
			case LoadError(message) => Left(message)
			case LoadSetupError(message) => Left(message)
			case LoadSetupDeclined => Left("Setup declined")
		}
}