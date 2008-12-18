/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.{File, FileFilter}
import scala.collection.Set
import scala.collection.mutable.{HashSet, ListBuffer}

private[sbt] object ClasspathUtilities
{
	def isArchive(path: Path): Boolean = isArchive(path.asFile)
	def isArchive(file: File): Boolean = isArchiveName(file.getName)
	def isArchiveName(fileName: String) = fileName.endsWith(".jar") || fileName.endsWith(".zip")
	// (jars, dirs)
	def separate(paths: Iterable[File]): (Iterable[File], Iterable[File]) = paths.partition(isArchive)
	def separatePaths(paths: Iterable[Path]) = separate(paths.map(_.asFile.getCanonicalFile))
	def buildSearchPaths(classpath: Iterable[Path]): (Set[File], Set[File]) =
	{
		val (jars, dirs) = separatePaths(classpath)
		(linkedSet(jars ++ extraJars), linkedSet(dirs ++ extraDirs))
	}
	def onClasspath(classpathJars: Set[File], classpathDirectories: Iterable[File], file: File): Boolean =
	{
		val f = file.getCanonicalFile
		if(ClasspathUtilities.isArchive(f))
			classpathJars.contains(f)
		else
			classpathDirectories.find(Path.relativize(_, f).isDefined).isDefined
	}
	
	private lazy val (extraJars, extraDirs) =
	{
		import scala.tools.nsc.GenericRunnerCommand
		val settings = (new GenericRunnerCommand(Nil, message => error(message))).settings
		val bootPaths = FileUtilities.pathSplit(settings.bootclasspath.value).map(p => new File(p)).toList
		val (bootJars, bootDirs) = separate(bootPaths)
		val extJars =
		{
			val buffer = new ListBuffer[File]
			def findJars(dir: File)
			{
				buffer ++= dir.listFiles(new FileFilter { def accept(f: File) = isArchive(f) })
				for(dir <- dir.listFiles(new FileFilter { def accept(f: File) = f.isDirectory }))
					findJars(dir)
			}
			for(path <- FileUtilities.pathSplit(settings.extdirs.value); val dir = new File(path) if dir.isDirectory)
				findJars(dir)
			buffer.readOnly.map(_.getCanonicalFile)
		}
		(linkedSet(extJars ++ bootJars), linkedSet(bootDirs))
	}
	private def linkedSet[T](s: Iterable[T]): Set[T] =
	{
		import scala.collection.jcl.Conversions.convertSet
		val set: scala.collection.mutable.Set[T] = new java.util.LinkedHashSet[T]
		set ++= s
		set.readOnly
	}
}