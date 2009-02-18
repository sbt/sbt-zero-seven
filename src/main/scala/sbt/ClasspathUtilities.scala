/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.File
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
				buffer ++= dir.listFiles(new SimpleFileFilter(isArchive))
				for(dir <- dir.listFiles(DirectoryFilter))
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

import java.net.{URL, URLClassLoader}
private abstract class LoaderBase(urls: Array[URL], parent: ClassLoader) extends URLClassLoader(urls, parent) with NotNull
{
	require(parent != null) // included because a null parent is legitimate in Java
	@throws(classOf[ClassNotFoundException])
	override final def loadClass(className: String, resolve: Boolean): Class[_] =
	{
		val loaded = findLoadedClass(className)
		val found =
			if(loaded == null)
				doLoadClass(className)
			else
				loaded
			
		if(resolve)
			resolveClass(found)
		found
	}
	protected def doLoadClass(className: String): Class[_]
	protected final def selfLoadClass(className: String): Class[_] = super.loadClass(className, false)
}
private class IntermediateLoader(urls: Array[URL], parent: ClassLoader) extends LoaderBase(urls, parent) with NotNull
{
	def doLoadClass(className: String): Class[_] =
	{
		// if this loader is asked to load an sbt class, it must be because the project we are building is sbt itself,
		 // so we want to load the version of classes on the project classpath, not the parent
		if(className.startsWith(Loaders.SbtPackage))
			findClass(className)
		else
			selfLoadClass(className)
	}
}
private class FilteredLoader(urls: Array[URL], parent: ClassLoader, filter: ClassFilter) extends URLClassLoader(urls, parent) with NotNull
{
	def this(urls: Array[URL], parent: ClassLoader, includePackages: Iterable[String]) =
		this(urls, parent, new ClassFilter { def include(className: String) = includePackages.exists(className.startsWith) })
	require(parent != null) // included because a null parent is legitimate in Java
	@throws(classOf[ClassNotFoundException])
	override final def loadClass(className: String, resolve: Boolean): Class[_] =
	{
		if(filter.include(className))
			super.loadClass(className, resolve)
		else
		{
			val loaded = parent.loadClass(className)
			if(resolve)
				resolveClass(loaded)
			loaded
		}
	}
}
private trait ClassFilter
{
	def include(className: String): Boolean
}
private class LazyFrameworkLoader(runnerClassName: String, urls: Array[URL], parent: ClassLoader, grandparent: ClassLoader)
	extends LoaderBase(urls, parent) with NotNull
{
	def doLoadClass(className: String): Class[_] =
	{
		if(Loaders.isNestedOrSelf(className, runnerClassName))
			findClass(className)
		else if(className.startsWith(Loaders.SbtPackage)) // we circumvent the parent loader because we know that we want the
			grandparent.loadClass(className)              // version of sbt that is currently the builder (not the project being built)
		else
			parent.loadClass(className)
	}
}
private object Loaders
{
	val SbtPackage = "sbt."
	def isNestedOrSelf(className: String, checkAgainst: String) =
		className == checkAgainst || className.startsWith(checkAgainst + "$")
}