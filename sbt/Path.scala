/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import Path._
import FileUtilities.wrapNull
import java.io.{File, FileFilter}
import scala.collection.mutable.{Buffer, ListBuffer}

sealed abstract class Path extends PathFinder with NotNull
{
	def ## : Path = new BaseDirectory(this)
	private[sbt] def addTo(buffer: Buffer[Path]) { buffer += this }
	override def / (component: String): Path = if(component == ".") this else new RelativePath(this, component)
	private[sbt] def asFile: File
	private[sbt] def asURL = asFile.toURI.toURL
	private[sbt] def relativePath: String
	private[sbt] def prependTo(s: String): String
	
	override final def equals(other: Any) =
		other match
		{
			case op: Path => asFile == op.asFile
			case _ => false
		}
	override final def hashCode = asFile.hashCode
}

private final case class BaseDirectory(private[sbt] val path: Path) extends Path
{
	for(existingBase <- baseAncestor(path))
		throw new IllegalArgumentException("Path " + path + " already has base component " + existingBase)
	
	override def toString = path.toString
	def asFile = path.asFile
	private[sbt] def relativePath = ""
	private[sbt] def prependTo(s: String) = "." + sep + s
}
private[sbt] final class ProjectDirectory(private[sbt] val asFile: File) extends Path
{
	override def toString = "."
	private[sbt] def relativePath = ""
	private[sbt] def prependTo(s: String) = "." + sep + s
}
private[sbt] final class RelativePath(val parentPath: Path, val component: String) extends Path
{
	checkComponent(component)
	override def toString = parentPath prependTo component
	private[sbt] lazy val asFile = new File(parentPath.asFile, component)
	private[sbt] def prependTo(s: String) =  parentPath prependTo (component + sep + s)
	private[sbt] lazy val relativePath =
	{
		val parentRelative = parentPath.relativePath
		if(parentRelative.isEmpty)
			component
		else
			parentRelative + sep + component
	}
}
object Path
{
	import java.io.File
	import File.pathSeparator
	
	def makeString(paths: Iterable[Path]): String = paths.map(_.asFile.getAbsolutePath).mkString(pathSeparator)
	
	def makeRelativeString(paths: Iterable[Path]): String = paths.map(_.relativePath).mkString(pathSeparator)
	def splitString(projectPath: Path, value: String): Iterable[Path] =
	{
		for(pathString <- FileUtilities.pathSplit(value) if pathString.length > 0) yield
			Path.fromString(projectPath, pathString)
	}
	
	def lazyPathFinder(paths: => Iterable[Path]): PathFinder =
		new PathFinder
		{
			private[sbt] def addTo(buffer: Buffer[Path]) = buffer ++= paths
		}
		
	val sep = java.io.File.separatorChar
	
	def checkComponent(c: String): String =
	{
		require(c.indexOf('/') == -1, "Path component '" + c + "' must not have forward slashes in it")
		require(c.indexOf('\\') == -1, "Path component '" + c + "' must not have backslashes in it")
		require(c != "..", "Path component cannot be '..'")
		require(c != ".", "Path component cannot be '.'")
		c
	}
	def fromString(projectPath: Path, value: String): Path =
	{
		val components = value.split("""[/\\]""")
		for(component <- components)
			checkComponent(component)
		(projectPath /: components)( (path, component) => path / component )
	}
	def baseAncestor(path: Path): Option[Path] =
		path match
		{
			case pd: ProjectDirectory => None
			case rp: RelativePath => baseAncestor(rp.parentPath)
			case b: BaseDirectory => Some(b.path)
		}
	
	def relativize(basePath: Path, path: Path): Option[Path] = relativize(basePath, path.asFile)
	def relativize(basePath: Path, file: File): Option[Path] = relativize(basePath, basePathString(basePath), file)
	def relativize(basePath: Path, basePathString: String, file: File): Option[Path] =
	{
		val pathString = file.getCanonicalPath
		if(pathString.startsWith(basePathString))
			Some(Path.fromString(basePath, pathString.substring(basePathString.length)))
		else
			None
	}
	private[sbt] def relativize(baseFile: File, file: File): Option[String] =
	{
		val pathString = file.getCanonicalPath
		val baseString = baseFileString(baseFile)
		if(pathString.startsWith(baseString))
			Some(pathString.substring(baseString.length))
		else
			None
	}
	def basePathString(basePath: Path): String = baseFileString(basePath.asFile)
	private def baseFileString(baseFile: File): String =
	{
		require(baseFile.isDirectory)
		
		val cp = baseFile.getCanonicalPath
		assert(cp.length > 0)
		if(cp.charAt(cp.length - 1) == File.separatorChar)
			cp
		else
			cp + File.separatorChar
	}
}

sealed abstract class PathFinder extends NotNull
{
	def +++(paths: PathFinder): PathFinder = new Paths(this, paths)
	def ***(filter: NameFilter): PathFinder = new DescendentPathFinder(this, filter, false)
	def **(filter: NameFilter): PathFinder = new DescendentPathFinder(this, filter, true)
	def *(filter: NameFilter): PathFinder = new ChildPathFinder(this, filter)
	def / (literal: String): PathFinder = new ChildPathFinder(this, new ExactFilter(literal))
	final def \ (literal: String): PathFinder = this / literal
	
	final def get: Iterable[Path] =
	{
		val buffer = new ListBuffer[Path]
		addTo(buffer)
		buffer.readOnly
	}
	private[sbt] def addTo(buffer: Buffer[Path])
}
private abstract class FilterPath extends PathFinder with FileFilter
{
	def parent: PathFinder
	def filter: NameFilter
	final def accept(file: File) = filter.accept(file.getName)
	
	protected object DirectoryFilter extends FileFilter
	{
		def accept(file: File) = file.isDirectory && FilterPath.this.accept(file)
	}
	protected def handlePath(path: Path, buffer: Buffer[Path])
	{
		for(matchedFile <- wrapNull(path.asFile.listFiles(this)))
			buffer += path / matchedFile.getName
	}
}
private class DescendentPathFinder(val parent: PathFinder, val filter: NameFilter, val includeSelf: Boolean) extends FilterPath
{
	private[sbt] def addTo(buffer: Buffer[Path])
	{
		for(path <- parent.get)
		{
			if(includeSelf && accept(path.asFile))
				buffer += path
			handlePathDescendent(path, buffer)
		}
	}
	private def handlePathDescendent(path: Path, buffer: Buffer[Path])
	{
		handlePath(path, buffer)
		for(childDirectory <- wrapNull(path.asFile.listFiles(DirectoryFilter)))
			handlePathDescendent(path / childDirectory.getName, buffer)
	}
}
private class ChildPathFinder(val parent: PathFinder, val filter: NameFilter) extends FilterPath
{
	private[sbt] def addTo(buffer: Buffer[Path])
	{
		for(path <- parent.get)
			handlePath(path, buffer)
	}
}
private class Paths(a: PathFinder, b: PathFinder) extends PathFinder
{
	private[sbt] def addTo(buffer: Buffer[Path])
	{
		a.addTo(buffer)
		b.addTo(buffer)
	}
}