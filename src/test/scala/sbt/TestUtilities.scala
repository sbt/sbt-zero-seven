/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.File
import FileUtilities._

object TestUtilities
{
	Project.log.setLevel(Level.Warn)
	
	// The returned directory is not actually read-only, but it should be treated that way
	def readOnlyResourceDirectory(group: String, name: String): Either[String, File] =
	{
		val groupDirectory = new File(testResourcesDirectory, group)
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
	{
		def readWrite(readOnly: File)(temporary: File): Either[String, T] =
		{
			val readWriteDirectory = new File(temporary, readOnly.getName)
			FileUtilities.copyDirectory(readOnly, readWriteDirectory, log)
			withDirectory(readWriteDirectory)
		}
		readOnlyResourceDirectory(group, name).right flatMap
			 { readOnly => doInTemporaryDirectory(log)(readWrite(readOnly)) }
	}
	
	def withProject[T](group: String, name: String)(f: Project => Either[String, T]): Either[String, T] =
		readWriteResourceDirectory(group, name, Project.log)
			{ dir => resultToEither(Project.loadProject(dir, Nil, None)).right.flatMap(f) }

	def resultToEither(result: LoadResult): Either[String, Project] =
		result match
		{
			case LoadSuccess(project) => Right(project)
			case LoadError(message) => Left(message)
			case LoadSetupError(message) => Left(message)
			case LoadSetupDeclined => Left("Setup declined")
		}
	
	val testResourcesDirectory = new File(System.getProperty("sbt.test.resources"))
}