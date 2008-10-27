/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.util.Properties
import ProjectAnalysis._
import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map, Set}

final class ProjectAnalysis(analysisPath: Path, projectPath: Path, log: Logger) extends NotNull
{
	private val dependencies = new HashMap[Path, Set[Path]]
	private val tests = new HashMap[Path, Set[String]]
	private val generatedClasses = new HashMap[Path, Set[Path]]
	private val projectDefinitions = new HashMap[Path, Set[String]]
	private val externalDependencies = new HashMap[File, Set[Path]]
	private def maps = List(dependencies, tests, generatedClasses, projectDefinitions)
	
	def clear() =
	{
		for(map <- maps)
			map.clear
		externalDependencies.clear
	}
	def removeSource(source: Path)
	{
		for(classes <- generatedClasses.get(source))
			FileUtilities.clean(classes, true, log)
		for(map <- maps)
			map -= source
	}
	def removeSelfDependency(source: Path)
	{
		for(deps <- dependencies.get(source))
			deps -= source
	}
	def removeDependent(source: Path)
	{
		for(deps <- dependencies.values)
			deps -= source
		for(deps <- externalDependencies.values)
			deps -= source
	}
	def removeDependencies(source: Path) = dependencies.removeKey(source)
	def removeExternalDependency(dep: File) = externalDependencies.removeKey(dep)
	
	def getDependencies(source: Path) = dependencies.get(source)
	def getClasses(sources: Iterable[Path]): Iterable[Path] =
	{
		val buffer = new ListBuffer[Path]
		for(source <- sources; classes <- generatedClasses.get(source))
			buffer ++= classes
		buffer.readOnly
	}
	def getClasses(source: Path) = generatedClasses.get(source)
	
	def allSources = dependencies.keys
	def allTests = all(tests)
	def allClasses = all(generatedClasses)
	def allProjects = all(projectDefinitions)
	def allExternalDependencies = readOnlyIterable(externalDependencies)
	def allDependencies = readOnlyIterable(dependencies)
	
	private def readOnlyIterable[Key, Value](i: Map[Key, Set[Value]]): Iterable[(Key, scala.collection.Set[Value])] =
		for( (key, set) <- i.elements.toList) yield (key, set.readOnly)
	
	def addTest(source: Path, testClassName: String) = add(source, testClassName, tests)
	def addSourceDependency(on: Path, from: Path) = add(on, from, dependencies)
	def addExternalDependency(on: File, from: Path) = add(on, from, externalDependencies)
	def addGeneratedClass(source: Path, file: Path) = add(source, file, generatedClasses)
	def addProjectDefinition(source: Path, className: String) = add(source, className, projectDefinitions)
	def markSource(source: Path) =
	{
		mark(source, dependencies)
		mark(source, generatedClasses)
	}
	
	def load(): Option[String] =
	{
		loadPaths(dependencies, analysisPath / DependenciesFileName, projectPath, log) orElse
			loadStrings(tests, analysisPath / TestsFileName, projectPath, log) orElse
			loadPaths(generatedClasses, analysisPath / GeneratedFileName, projectPath, log) orElse
			loadStrings(projectDefinitions, analysisPath / ProjectDefinitionsName, projectPath, log) orElse
			loadFilePaths(externalDependencies, analysisPath / ExternalDependenciesFileName, projectPath, log)
	}
	
	def save(): Option[String] =
	{
		FileUtilities.createDirectory(analysisPath.asFile, log) orElse
			writePaths(dependencies, DependenciesLabel, analysisPath / DependenciesFileName, log) orElse
			writeStrings(tests, TestsLabel, analysisPath / TestsFileName, log) orElse
			writePaths(generatedClasses, GeneratedLabel, analysisPath / GeneratedFileName, log) orElse
			writeStrings(projectDefinitions, ProjectDefinitionsLabel, analysisPath / ProjectDefinitionsName, log) orElse
			writeFilePaths(externalDependencies, ExternalDependenciesLabel, analysisPath / ExternalDependenciesFileName, log)
	}
	
}
object ProjectAnalysis
{	
	val GeneratedFileName = "generated_files"
	val DependenciesFileName = "dependencies"
	val TestsFileName = "tests"
	val ProjectDefinitionsName = "projects"
	val ExternalDependenciesFileName = "external"
	
	val GeneratedLabel = "Generated Classes"
	val DependenciesLabel = "Source Dependencies"
	val TestsLabel = "Tests"
	val ProjectDefinitionsLabel = "Project Definitions"
	val ExternalDependenciesLabel = "External Dependencies"
	
	private[sbt] def write(properties: Properties, label: String, to: Path, log: Logger) =
		FileUtilities.writeStream(to.asFile, log)((output: OutputStream) => { properties.store(output, label); None })
	
	private[sbt] def load(properties: Properties, from: Path, log: Logger): Option[String] =
	{
		val file = from.asFile
		if(file.exists)
			FileUtilities.readStream(file, log)( (input: InputStream) => { properties.load(input); None })
		else
			None
	}
	private[sbt] def mark(source: Path, map: HashMap[Path, Set[Path]])
	{
		if(!map.contains(source))
			map.put(source, new HashSet[Path])
	}
	
	def load(analysisPath: Path, projectPath: Path, log: Logger): Either[String, ProjectAnalysis] =
	{
		val analysis = new ProjectAnalysis(analysisPath, projectPath, log)
		analysis.load().toLeft(analysis)
	}
	private[sbt] def all[Value](map: Map[Path, Set[Value]]): Iterable[Value] =
		map.values.toList.flatMap(set => set.toList)
	
	private[sbt] def add[Key, Value](key: Key, value: Value, map: Map[Key, Set[Value]])
	{
		map.getOrElseUpdate(key, new HashSet[Value]) + value
	}
	
	import java.util.Properties
	private[sbt] def writeStrings(map: Map[Path, Set[String]], label: String, to: Path, log: Logger) =
		write(map, label, (i: Iterable[String]) => i.mkString(File.pathSeparator), to, log)
	private[sbt] def writePaths(map: Map[Path, Set[Path]], label: String, to: Path, log: Logger) =
		write(map, label, Path.makeRelativeString, to, log)
	private[sbt] def write[Value](map: Map[Path, Set[Value]], label: String,
		valuesToString: Iterable[Value] => String, to: Path, log: Logger): Option[String] =
	{
		val properties = new Properties
		for( (path, set) <- map)
			properties.setProperty(path.relativePath, valuesToString(set))
		write(properties, label, to, log)
	}
	private[sbt] def writeFilePaths(map: Map[File, Set[Path]], label: String, to: Path, log: Logger) =
	{
		val properties = new Properties
		for( (file, set) <- map)
			properties.setProperty(file.getCanonicalPath, Path.makeRelativeString(set))
		write(properties, label, to, log)
	}
	
	private def pathSetFromString(projectPath: Path)(s: String): Set[Path] =
		(new HashSet[Path]) ++ Path.splitString(projectPath, s)
	
	private[sbt] def loadStrings(map: Map[Path, Set[String]], from: Path, projectPath: Path, log: Logger) =
		load(map, (s: String) => (new HashSet[String]) ++ FileUtilities.pathSplit(s), from, projectPath, log)
	private[sbt] def loadPaths(map: Map[Path, Set[Path]], from: Path, projectPath: Path, log: Logger) =
		load(map, pathSetFromString(projectPath)(_), from, projectPath, log)
	private[sbt] def load[Value](map: Map[Path, Set[Value]], stringToSet: String => Set[Value],
		from: Path, projectPath: Path, log: Logger): Option[String] =
	{
		map.clear
		val properties = new Properties
		load(properties, from, log) orElse
		{
			for(name <- propertyNames(properties))
				map.put(Path.fromString(projectPath, name), stringToSet(properties.getProperty(name)))
			None
		}
	}
	private[sbt] def loadFilePaths(map: Map[File, Set[Path]], from: Path, projectPath: Path, log: Logger) =
	{
		map.clear
		val properties = new Properties
		load(properties, from, log) orElse
		{
			for(name <- propertyNames(properties))
				map.put(new File(name), pathSetFromString(projectPath)(properties.getProperty(name)))
			None
		}
	}
	private def propertyNames(properties: Properties): Iterable[String] =
	{
		import java.util.Collections.list
		import scala.collection.jcl.Conversions.convertList
		convertList(list(properties.propertyNames)).map(_.toString)
	}
}