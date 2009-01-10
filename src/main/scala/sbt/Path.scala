/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import Path._
import FileUtilities.wrapNull
import java.io.{File, FileFilter}
import scala.collection.mutable.{Set, HashSet}

/** A Path represents a file in a project.
* @see sbt.PathFinder*/
sealed abstract class Path extends PathFinder with NotNull
{
	/** Creates a base directory for this path.  This is used by copy and zip functions
	* to determine the relative path that should be used in the destination.  For example,
	* if the following path is specified to be copied to directory 'd',
	* 
	* <code>((a / b) ##) / x / y</code>
	*
	* the copied path would be 
	*
	* <code>d / x / y</code>
	*
	* The <code>relativePath</code> method is used to return the relative path to the base directory. */
	def ## : Path = new BaseDirectory(this)
	private[sbt] def addTo(pathSet: Set[Path])
	{
		if(asFile.exists)
			pathSet += this
	}
	override def / (component: String): Path = if(component == ".") this else new RelativePath(this, component)
	/** True if and only if the file represented by this path exists.*/
	def exists = asFile.exists
	/** True if and only if the file represented by this path is a directory.*/
	def isDirectory = asFile.isDirectory
	/** The last modified time of the file represented by this path.*/
	def lastModified = asFile.lastModified
	/* True if and only if file that this path represents exists and the file represented by the path 'p'
	* does not exist or was modified before the file for this path.*/
	def newerThan(p: Path): Boolean = exists && (!p.exists || lastModified > p.lastModified)
	/* True if and only if file that this path represents does not exist or the file represented by the path 'p'
	* exists and was modified after the file for this path.*/
	def olderThan(p: Path): Boolean = p newerThan this
	/** The file represented by this path.*/
	def asFile: File
	/** The file represented by this path converted to a <code>URL</code>.*/
	def asURL = asFile.toURI.toURL
	/** The string representation of this path relative to the base directory.  The project directory is the
	* default base directory if one is not specified explicitly using the <code>##</code> operator.*/
	def relativePath: String
	private[sbt] def prependTo(s: String): String
	
	/** Equality of Paths is defined in terms of the underlying <code>File</code>.*/
	override final def equals(other: Any) =
		other match
		{
			case op: Path => asFile == op.asFile
			case _ => false
		}
	/** The hash code of a Path is that of the underlying <code>File</code>.*/
	override final def hashCode = asFile.hashCode
}

private final case class BaseDirectory(private[sbt] val path: Path) extends Path
{
	override def toString = path.toString
	def asFile = path.asFile
	def relativePath = ""
	private[sbt] def prependTo(s: String) = "." + sep + s
}
private[sbt] final class ProjectDirectory(val asFile: File) extends Path
{
	override def toString = "."
	def relativePath = ""
	private[sbt] def prependTo(s: String) = "." + sep + s
}
private[sbt] final class RelativePath(val parentPath: Path, val component: String) extends Path
{
	checkComponent(component)
	override def toString = parentPath prependTo component
	lazy val asFile = new File(parentPath.asFile, component)
	private[sbt] def prependTo(s: String) =  parentPath prependTo (component + sep + s)
	lazy val relativePath =
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
	
	/** Constructs a String representation of <code>Path</code>s.  The absolute path String of each <code>Path</code> is
	* separated by the platform's path separator.*/
	def makeString(paths: Iterable[Path]): String = paths.map(_.asFile.getAbsolutePath).mkString(pathSeparator)
	
	/** Constructs a String representation of <code>Path</code>s.  The relative path String of each <code>Path</code> is
	* separated by the platform's path separator.*/
	def makeRelativeString(paths: Iterable[Path]): String = paths.map(_.relativePath).mkString(pathSeparator)
	
	def splitString(projectPath: Path, value: String): Iterable[Path] =
	{
		for(pathString <- FileUtilities.pathSplit(value) if pathString.length > 0) yield
			Path.fromString(projectPath, pathString)
	}
	
	/** A <code>PathFinder</code> that always produces the empty set of <code>Path</code>s.*/
	def emptyPathFinder =
		new PathFinder
		{
			private[sbt] def addTo(pathSet: Set[Path]) {}
		}
	/** A <code>PathFinder</code> that selects the paths provided by the <code>paths</code> argument, which is
	* reevaluated on each call to the <code>PathFinder</code>'s <code>get</code> method.  */
	def lazyPathFinder(paths: => Iterable[Path]): PathFinder =
		new PathFinder
		{
			private[sbt] def addTo(pathSet: Set[Path]) = pathSet ++= paths
		}
		
	/** The separator character of the platform.*/
	val sep = java.io.File.separatorChar
	
	/** Checks the string to verify that it is a legal path component.  The string must be non-empty,
	* not a slash, and not '.' or '..'.*/
	def checkComponent(c: String): String =
	{
		require(c.length > 0, "Path component must not be empty")
		require(c.indexOf('/') == -1, "Path component '" + c + "' must not have forward slashes in it")
		require(c.indexOf('\\') == -1, "Path component '" + c + "' must not have backslashes in it")
		require(c != "..", "Path component cannot be '..'")
		require(c != ".", "Path component cannot be '.'")
		c
	}
	/** Converts a path string relative to the given base path to a <code>Path</code>. */
	def fromString(basePath: Path, value: String): Path =
	{
		if(value.isEmpty)
			basePath
		else
		{
			val components = value.split("""[/\\]""")
			(basePath /: components)( (path, component) => path / component )
		}
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
			Some(fromString(basePath, pathString.substring(basePathString.length)))
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
	private[sbt] def basePathString(basePath: Path): Option[String] = baseFileString(basePath.asFile)
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

/** A path finder constructs a set of paths.  The set is evaluated by a call to the <code>get</code>
* method.  The set will be different for different calls to <code>get</code> if the underlying filesystem
* has changed.*/
sealed abstract class PathFinder extends NotNull
{
	/** The union of the paths found by this <code>PathFinder</code> with the paths found by 'paths'.*/
	def +++(paths: PathFinder): PathFinder = new Paths(this, paths)
	/** Excludes all paths from <code>excludePaths</code> from the paths selected by this <code>PathFinder</code>.*/
	def ---(excludePaths: PathFinder): PathFinder = new ExcludePaths(this, excludePaths)
	/** Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
	* descendents of paths selected by this finder.*/
	def **(filter: NameFilter): PathFinder = new DescendentOrSelfPathFinder(this, filter)
	/** Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
	* immediate children of paths selected by this finder.*/
	def *(filter: NameFilter): PathFinder = new ChildPathFinder(this, filter)
	/** Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
	* of paths selected by this finder.*/
	def / (literal: String): PathFinder = new ChildPathFinder(this, new ExactFilter(literal))
	/** Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
	* of paths selected by this finder.*/
	final def \ (literal: String): PathFinder = this / literal

	/** Selects all descendent paths with a name that matches <code>include</code> and do not have an intermediate
	* path with a name that matches <code>intermediateExclude</code>.  Typical usage is:
	*
	* <code>descendentsExcept("*.jar", ".svn")</code>*/
	def descendentsExcept(include: NameFilter, intermediateExclude: NameFilter): PathFinder =
		(this ** include) --- (this ** intermediateExclude ** include)
	
	/** Evaluates this finder.  The set returned by this method will reflect the underlying filesystem at the
	* time of calling.  If the filesystem changes, two calls to this method might be different.*/
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
	
	protected def handlePath(path: Path, pathSet: Set[Path])
	{
		for(matchedFile <- wrapNull(path.asFile.listFiles(this)))
			pathSet += path / matchedFile.getName
	}
}
private class DescendentOrSelfPathFinder(val parent: PathFinder, val filter: NameFilter) extends FilterPath
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		for(path <- parent.get)
		{
			if(accept(path.asFile))
				pathSet += path
			handlePathDescendent(path, pathSet)
		}
	}
	protected object DirectoryFilter extends FileFilter
	{
		def accept(file: File) = file.isDirectory
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
private class ExcludePaths(include: PathFinder, exclude: PathFinder) extends PathFinder
{
	private[sbt] def addTo(pathSet: Set[Path])
	{
		val includeSet = new HashSet[Path]
		include.addTo(includeSet)
		
		val excludeSet = new HashSet[Path]
		exclude.addTo(excludeSet)
		
		includeSet --= excludeSet
		pathSet ++= includeSet
	}
}