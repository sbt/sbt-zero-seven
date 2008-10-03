package sbt

import java.io.{File, FileReader, FileWriter, Reader, Writer}
import java.util.Properties
import ProjectAnalysis._
import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map, Set}

final class ProjectAnalysis
{
	private val dependencies = new HashMap[Path, Set[Path]]
	private val tests = new HashMap[Path, Set[String]]
	private val generatedClasses = new HashMap[Path, Set[Path]]
	private def allMaps = List(dependencies, tests, generatedClasses)
	
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
	
	def addTest(source: Path, testClassName: String) = add(source, testClassName, tests)
	def addDependency(on: Path, from: Path) = add(on, from, dependencies)
	def addGeneratedClass(source: Path, file: Path) = add(source, file, generatedClasses)
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
			loadPaths(generatedClasses, analysisPath / GeneratedFileName, info, log)
	}
	
	def save(info: ProjectInfo, log: Logger): Option[String] =
	{
		val analysisPath = getAnalysisPath(info)
		FileUtilities.createDirectory(analysisPath.asFile, log) orElse
			writePaths(dependencies, DependenciesLabel, analysisPath / DependenciesFileName, log) orElse
			writeStrings(tests, TestsLabel, analysisPath / TestsFileName, log) orElse
			writePaths(generatedClasses, GeneratedLabel, analysisPath / GeneratedFileName, log)
	}
	
	import java.util.Properties
	private def writeStrings(map: Map[Path, Set[String]], label: String, to: Path, log: Logger) =
		write(map, label, (i: Iterable[String]) => i.mkString(File.pathSeparator), to, log)
	private def writePaths(map: Map[Path, Set[Path]], label: String, to: Path, log: Logger) =
		write(map, label, Path.makeRelativeString, to, log)
	private def write[Value](map: Map[Path, Set[Value]], label: String, valuesToString: Iterable[Value] => String, to: Path, log: Logger) =
	{
		val properties = new Properties
		for( (path, set) <- map)
			properties.setProperty(path.relativePath, valuesToString(set))
		ProjectAnalysis.write(properties, label, to, log)
	}
	
	private def loadStrings(map: Map[Path, Set[String]], from: Path, info: ProjectInfo, log: Logger) =
		load(map, (s: String) => (new HashSet[String]) ++ FileUtilities.pathSplit(s), from, info, log)
	private def loadPaths(map: Map[Path, Set[Path]], from: Path, info: ProjectInfo, log: Logger) =
		load(map, (s: String) => (new HashSet[Path]) ++ Path.splitString(info.projectPath, s), from, info, log)
	private def load[Value](map: Map[Path, Set[Value]], stringToSet: String => Set[Value], from: Path, info: ProjectInfo, log: Logger) =
	{
		map.clear
		val properties = new Properties
		ProjectAnalysis.load(properties, from, log) orElse
		{
			val base = info.projectPath
			for(name <- scala.collection.jcl.Set(properties.stringPropertyNames))
			{
				val key = Path.fromString(base, name)
				map.put(key, stringToSet(properties.getProperty(name)))
			}
			None
		}
	}
}
object ProjectAnalysis
{
	val AnalysisDirectoryName = "analysis"
	
	val GeneratedFileName = "generated_files"
	val DependenciesFileName = "dependencies"
	val TestsFileName = "tests"
	
	val GeneratedLabel = "Generated Classes"
	val DependenciesLabel = "Source Dependencies"
	val TestsLabel = "Tests"
	
	val NoTimestamp = -1
	
	private def write(properties: Properties, label: String, to: Path, log: Logger) =
		FileUtilities.write(to.asFile, log)((writer: Writer) => { properties.store(writer, label); None })
	
	private def load(properties: Properties, from: Path, log: Logger): Option[String] =
	{
		val file = from.asFile
		if(file.exists)
			FileUtilities.read(file, log)( (reader: Reader) => { properties.load(reader); None })
		else
			None
	}
	private def mark(source: Path, map: HashMap[Path, Set[Path]])
	{
		if(!map.contains(source))
			map.put(source, new HashSet[Path])
	}
	
	def load(info: ProjectInfo, log: Logger): Either[String, ProjectAnalysis] =
	{
		val analysis = new ProjectAnalysis
		analysis.load(info, log).toLeft(analysis)
	}
	private def all[Value](map: Map[Path, Set[Value]]): Iterable[Value] =
		map.values.toList.flatMap(set => set.toList)
	
	private def add[Value](key: Path, value: Value, map: Map[Path, Set[Value]])
	{
		map.getOrElseUpdate(key, new HashSet[Value]) + value
	}
}