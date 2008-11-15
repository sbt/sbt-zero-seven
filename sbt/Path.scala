/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import Path._
import FileUtilities.wrapNull
import java.io.{File, FileFilter}
import scala.collection.mutable.{Set, HashSet}

sealed abstract class Path extends PathFinder with NotNull
{
	def ## : Path = new BaseDirectory(this)
	private[sbt] def addTo(pathSet: Set[Path]) { pathSet += this }
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
	
	def emptyPathFinder =
		new PathFinder
		{
			private[sbt] def addTo(pathSet: Set[Path]) {}
		}
	def lazyPathFinder(paths: => Iterable[Path]): PathFinder =
		new PathFinder
		{
			private[sbt] def addTo(pathSet: Set[Path]) = pathSet ++= paths
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
	def relativize(basePath: Path, file: File): Option[Path] =
		basePathString(basePath) flatMap { baseString => relativize(basePath, baseString, file) }
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
		baseFileString(baseFile) flatMap
		{
			baseString =>
			{
				if(pathString.startsWith(baseString))
					Some(pathString.substring(baseString.length))
				else
					None
			}
		}
	}
	def basePathString(basePath: Path): Option[String] = baseFileString(basePath.asFile)
	private def baseFileString(baseFile: File): Option[String] =
	{
		if(baseFile.isDirectory)
		{
			val cp = baseFile.getCanonicalPath
			assert(cp.length > 0)
			if(cp.charAt(cp.length - 1) == File.separatorChar)
				Some(cp)
			else
				Some(cp + File.separatorChar)
		}
		else
			None
	}
}

sealed abstract class PathFinder extends NotNull
{
	/** The union of the paths found by this with the paths found by 'paths'.*/
	def +++(paths: PathFinder): PathFinder = new Paths(this, paths)
	def ***(filter: NameFilter): PathFinder = new DescendentPathFinder(this, filter, false)
	def **(filter: NameFilter): PathFinder = new DescendentPathFinder(this, filter, true)
	def *(filter: NameFilter): PathFinder = new ChildPathFinder(this, filter)
	def / (literal: String): PathFinder = new ChildPathFinder(this, new ExactFilter(literal))
	final def \ (literal: String): PathFinder = this / literal
	
	/** Evaluates this finder.*/
	final def get: scala.collection.Set[Path] =
	{
		val pathSet = new HashSet[Path]
		addTo(pathSet)
		pathSet.readOnly
	}
	private[sbt] def addTo(pathSet: Set[Path])
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
	protected def handlePath(path: Path, pathSet: Set[Path])
	{
		for(matchedFile <- wrapNull(path.asFile.listFiles(this)))
			pathSet += path / matchedFile.getName
	}
}
private class DescendentPathFinder(val parent: PathFinder, val filter: NameFilter, val includeSelf: Boolean) extends FilterPath
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		for(path <- parent.get)
		{
			if(includeSelf && accept(path.asFile))
				pathSet += path
			handlePathDescendent(path, pathSet)
		}
	}
	private def handlePathDescendent(path: Path, pathSet: Set[Path])
	{
		handlePath(path, pathSet)
		for(childDirectory <- wrapNull(path.asFile.listFiles(DirectoryFilter)))
			handlePathDescendent(path / childDirectory.getName, pathSet)
	}
}
private class ChildPathFinder(val parent: PathFinder, val filter: NameFilter) extends FilterPath
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		for(path <- parent.get)
			handlePath(path, pathSet)
	}
}
private class Paths(a: PathFinder, b: PathFinder) extends PathFinder
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		a.addTo(pathSet)
		b.addTo(pathSet)
	}
}