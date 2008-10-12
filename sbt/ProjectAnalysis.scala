/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.util.Properties
import ProjectAnalysis._
import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map, Set}

final class ProjectAnalysis extends NotNull
{
	private val dependencies = new HashMap[Path, Set[Path]]
	private val tests = new HashMap[Path, Set[String]]
	private val generatedClasses = new HashMap[Path, Set[Path]]
	private val projectDefinitions = new HashMap[Path, Set[String]]
	private def allMaps = List(dependencies, tests, generatedClasses, projectDefinitions)
	
	def clear =
		for(map <- allMaps)
			map.clear
	
	def removeSource(source: Path, log: Logger)
	{
		for(classes <- generatedClasses.get(source))
			FileUtilities.clean(classes, true, log)
		for(map <- allMaps)
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
	}
	def removeDependencies(source: Path) = dependencies.removeKey(source)
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
	
	def addTest(source: Path, testClassName: String) = add(source, testClassName, tests)
	def addDependency(on: Path, from: Path) = add(on, from, dependencies)
	def addGeneratedClass(source: Path, file: Path) = add(source, file, generatedClasses)
	def addProjectDefinition(source: Path, className: String) = add(source, className, projectDefinitions)
	def markSource(source: Path) =
	{
		mark(source, dependencies)
		mark(source, generatedClasses)
	}
	def getAnalysisPath(info: ProjectInfo) = info.builderPath / AnalysisDirectoryName
		
	def load(info: ProjectInfo, log: Logger): Option[String] =
	{
		val analysisPath = getAnalysisPath(info)
		loadPaths(dependencies, analysisPath / DependenciesFileName, info, log) orElse
			loadStrings(tests, analysisPath / TestsFileName, info, log) orElse
			loadPaths(generatedClasses, analysisPath / GeneratedFileName, info, log) orElse
			loadStrings(projectDefinitions, analysisPath / ProjectDefinitionsName, info, log)
	}
	
	def save(info: ProjectInfo, log: Logger): Option[String] =
	{
		val analysisPath = getAnalysisPath(info)
		FileUtilities.createDirectory(analysisPath.asFile, log) orElse
			writePaths(dependencies, DependenciesLabel, analysisPath / DependenciesFileName, log) orElse
			writeStrings(tests, TestsLabel, analysisPath / TestsFileName, log) orElse
			writePaths(generatedClasses, GeneratedLabel, analysisPath / GeneratedFileName, log) orElse
			writeStrings(projectDefinitions, ProjectDefinitionsLabel, analysisPath / ProjectDefinitionsName, log)
	}
	
}
object ProjectAnalysis
{
	val AnalysisDirectoryName = "analysis"
	
	val GeneratedFileName = "generated_files"
	val DependenciesFileName = "dependencies"
	val TestsFileName = "tests"
	val ProjectDefinitionsName = "projects"
	
	val GeneratedLabel = "Generated Classes"
	val DependenciesLabel = "Source Dependencies"
	val TestsLabel = "Tests"
	val ProjectDefinitionsLabel = "Project Definitions"
	
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
	
	def load(info: ProjectInfo, log: Logger): Either[String, ProjectAnalysis] =
	{
		val analysis = new ProjectAnalysis
		analysis.load(info, log).toLeft(analysis)
	}
	private[sbt] def all[Value](map: Map[Path, Set[Value]]): Iterable[Value] =
		map.values.toList.flatMap(set => set.toList)
	
	private[sbt] def add[Value](key: Path, value: Value, map: Map[Path, Set[Value]])
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
	
	private[sbt] def loadStrings(map: Map[Path, Set[String]], from: Path, info: ProjectInfo, log: Logger) =
		load(map, (s: String) => (new HashSet[String]) ++ FileUtilities.pathSplit(s), from, info, log)
	private[sbt] def loadPaths(map: Map[Path, Set[Path]], from: Path, info: ProjectInfo, log: Logger) =
		load(map, (s: String) => (new HashSet[Path]) ++ Path.splitString(info.projectPath, s), from, info, log)
	private[sbt] def load[Value](map: Map[Path, Set[Value]], stringToSet: String => Set[Value],
		from: Path, info: ProjectInfo, log: Logger): Option[String] =
	{
		map.clear
		val properties = new Properties
		load(properties, from, log) orElse
		{
			val base = info.projectPath
			import java.util.Collections.list
			import scala.collection.jcl.Conversions.convertList
			for(nameKey <- convertList(list(properties.propertyNames)))
			{
				val name = nameKey.toString
				val key = Path.fromString(base, name)
				map.put(key, stringToSet(properties.getProperty(name)))
			}
			None
		}
	}
}