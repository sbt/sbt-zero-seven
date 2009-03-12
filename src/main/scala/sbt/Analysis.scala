/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

trait TaskAnalysis[Source, Product, External] extends NotNull
{
	import scala.collection.Set
	def save(): Option[String]
	def revert(): Option[String]
	def clear(): Unit
	
	def allSources: Set[Source]
	def allProducts: Set[Product]
	def allExternals: Set[External]
	
	def sourceDependencies(source: Source): Option[Set[Source]]
	def products(source: Source): Option[Set[Product]]
	def externalDependencies(external: External): Option[Set[Source]]
	
	def addSource(source: Source): Unit
	def addExternalDependency(dependsOn: External, source: Source): Unit
	def addSourceDependency(dependsOn: Source, source: Source): Unit
	def addProduct(source: Source, product: Product): Unit
	
	def removeSource(source: Source): Unit
	def removeDependent(source: Source): Unit
	def removeDependencies(source: Source): Option[Set[Source]]
	def removeExternalDependency(external: External): Unit
}

import java.io.File
import BasicAnalysis._
import MapUtilities._
import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map, Set}

sealed class BasicAnalysis(analysisPath: Path, projectPath: Path, log: Logger) extends TaskAnalysis[Path, Path, File]
{
	private val sourceDependencyMap: Map[Path, Set[Path]] = new HashMap
	private val productMap: Map[Path, Set[Path]] = new HashMap
	private val externalDependencyMap: Map[File, Set[Path]] = new HashMap
	
	final type AnyMapToSource = Map[K, Set[Path]] forSome {type K}
	final type AnySourceMap = Map[Path, T] forSome {type T}
	final type AnySourceSetMap = Map[Path, Set[T]] forSome {type T}
	final type AnyMap = Map[K, V] forSome { type K; type V }
	
	protected def mapsToClear = List[AnyMap](sourceDependencyMap, productMap, externalDependencyMap)
	protected def mapsToRemoveSource = List[AnySourceMap](sourceDependencyMap, productMap)
	protected def mapsToRemoveDependent = List[AnyMapToSource](sourceDependencyMap, externalDependencyMap)
	protected def mapsToMark = List[AnySourceSetMap](sourceDependencyMap, productMap)
	
	def clear()
	{
		for(map <- mapsToClear)
			map.clear()
	}
	def removeSource(source: Path)
	{
		for(sourceProducts <- productMap.get(source))
			FileUtilities.clean(sourceProducts, true, log)
		for(map <- mapsToRemoveSource)
			map -= source
	}
	def removeSelfDependency(source: Path)
	{
		for(deps <- sourceDependencyMap.get(source))
			deps -= source
	}
	def removeDependent(source: Path)
	{
		for(map <- mapsToRemoveDependent; deps <- map.values)
			deps -= source
	}
	def removeDependencies(source: Path) = sourceDependencyMap.removeKey(source)
	def removeExternalDependency(dep: File) = externalDependencyMap.removeKey(dep)
	
	def externalDependencies(external: File) = externalDependencyMap.get(external)
	def sourceDependencies(source: Path) = sourceDependencyMap.get(source)
	def products(sources: Iterable[Path]): Iterable[Path] =
	{
		val buffer = new ListBuffer[Path]
		for(source <- sources; sourceProducts <- productMap.get(source))
			buffer ++= sourceProducts
		buffer.readOnly
	}
	def products(source: Path) = productMap.get(source)
	
	def allSources = sourceDependencyMap.keySet
	def allProducts: Set[Path] = HashSet(productMap.values.toList.flatten[Path].toSeq : _*)
	def allExternals = externalDependencyMap.keySet
	
	def allExternalDependencies = readOnlyIterable(externalDependencyMap)
	def allDependencies = readOnlyIterable(sourceDependencyMap)
	
	def addSourceDependency(on: Path, from: Path) = add(on, from, sourceDependencyMap)
	def addExternalDependency(on: File, from: Path) = add(on, from, externalDependencyMap)
	def addProduct(source: Path, file: Path) = add(source, file, productMap)
	def addSource(source: Path) =
	{
		for(map <- mapsToMark)
			mark(source, map)
	}
	
	def revert() = load()
	final def load(): Option[String] =
	{
		loadPaths(sourceDependencyMap, analysisPath / DependenciesFileName, projectPath, log) orElse
			loadPaths(productMap, analysisPath / GeneratedFileName, projectPath, log) orElse
			loadFilePaths(externalDependencyMap, analysisPath / ExternalDependenciesFileName, projectPath, log) orElse
			loadExtra()
	}
	protected def loadExtra(): Option[String] = None
	
	final def save(): Option[String] =
	{
		FileUtilities.createDirectory(analysisPath.asFile, log) orElse
			writePaths(sourceDependencyMap, DependenciesLabel, analysisPath / DependenciesFileName, log) orElse
			writePaths(productMap, GeneratedLabel, analysisPath / GeneratedFileName, log) orElse
			writeFilePaths(externalDependencyMap, ExternalDependenciesLabel, analysisPath / ExternalDependenciesFileName, log) orElse
			saveExtra()
	}
	protected def saveExtra(): Option[String] = None
}
object BasicAnalysis
{
	val GeneratedFileName = "generated_files"
	val DependenciesFileName = "dependencies"
	val ExternalDependenciesFileName = "external"
	
	val GeneratedLabel = "Generated Classes"
	val DependenciesLabel = "Source Dependencies"
	val ExternalDependenciesLabel = "External Dependencies"
	
	def load(analysisPath: Path, projectPath: Path, log: Logger): Either[String, BasicAnalysis] =
	{
		val analysis = new BasicAnalysis(analysisPath, projectPath, log)
		analysis.load().toLeft(analysis)
	}
}
object CompileAnalysis
{
	val HashesFileName = "hashes"
	val TestsFileName = "tests"
	val ProjectDefinitionsName = "projects"
	
	val HashesLabel = "Source Hashes"
	val TestsLabel = "Tests"
	val ProjectDefinitionsLabel = "Project Definitions"
	
	def load(analysisPath: Path, projectPath: Path, log: Logger): Either[String, CompileAnalysis] =
	{
		val analysis = new CompileAnalysis(analysisPath, projectPath, log)
		analysis.load().toLeft(analysis)
	}
}
final class CompileAnalysis(analysisPath: Path, projectPath: Path, log: Logger)
	extends BasicAnalysis(analysisPath, projectPath, log)
{
	import CompileAnalysis._
	private val testMap = new HashMap[Path, Set[TestDefinition]]
	private val projectDefinitionMap = new HashMap[Path, Set[String]]
	private val hashesMap = new HashMap[Path, Array[Byte]]
	
	override protected def mapsToClear = hashesMap :: testMap :: projectDefinitionMap :: super.mapsToClear
	override protected def mapsToRemoveSource = hashesMap :: testMap :: projectDefinitionMap :: super.mapsToRemoveSource
	
	def allTests = all(testMap)
	def allProjects = all(projectDefinitionMap)
	def testSourceMap: Map[String, Path] =
	{
		val map = new HashMap[String, Path]
		for( (source, tests) <- testMap; test <- tests) map(test.testClassName) = source
		map
	}
	
	def addTest(source: Path, test: TestDefinition) = add(source, test, testMap)
	def addProjectDefinition(source: Path, className: String) = add(source, className, projectDefinitionMap)
	def setHash(source: Path, hash: Array[Byte]) { hashesMap(source) = hash }
	def clearHash(source: Path) { hashesMap.removeKey(source) }
	def hash(source: Path) = hashesMap.get(source)
	def clearHashes() { hashesMap.clear() }
	
	def getClasses(sources: PathFinder, outputDirectory: Path): PathFinder =
		Path.lazyPathFinder
		{
			val basePath = (outputDirectory ##)
			for(c <- products(sources.get)) yield
				Path.relativize(basePath, c).getOrElse(c)
		}
		
	override protected def loadExtra() =
	{
		loadHashes(hashesMap, analysisPath / HashesFileName, projectPath, log) orElse
		loadTestDefinitions(testMap, analysisPath / TestsFileName, projectPath, log) orElse
		loadStrings(projectDefinitionMap, analysisPath / ProjectDefinitionsName, projectPath, log)
	}
	override protected def saveExtra() =
	{
		writeHashes(hashesMap, HashesLabel, analysisPath / HashesFileName, log) orElse
		writeTestDefinitions(testMap, TestsLabel, analysisPath / TestsFileName, log) orElse
		writeStrings(projectDefinitionMap, ProjectDefinitionsLabel, analysisPath / ProjectDefinitionsName, log)
	}
}

import java.util.Properties
import java.io.{FileInputStream, FileOutputStream, InputStream, OutputStream}
object PropertiesUtilities
{
	def write(properties: Properties, label: String, to: Path, log: Logger) =
		FileUtilities.writeStream(to.asFile, log)((output: OutputStream) => { properties.store(output, label); None })
	
	def load(properties: Properties, from: Path, log: Logger): Option[String] =
	{
		val file = from.asFile
		if(file.exists)
			FileUtilities.readStream(file, log)( (input: InputStream) => { properties.load(input); None })
		else
			None
	}
	
	def propertyNames(properties: Properties): Iterable[String] =
	{
		import java.util.Collections.list
		import scala.collection.jcl.Conversions.convertList
		convertList(list(properties.propertyNames)).map(_.toString)
	}
}
object MapUtilities
{
	def all[Key, Value](map: Map[Key, Set[Value]]): Iterable[Value] =
		map.values.toList.flatMap(set => set.toList)
	
	def readOnlyIterable[Key, Value](i: Map[Key, Set[Value]]): Iterable[(Key, scala.collection.Set[Value])] =
		for( (key, set) <- i.elements.toList) yield (key, set.readOnly)
		
	def mark[Key, Value](source: Key, map: Map[Key, Set[Value]])
	{
		if(!map.contains(source))
			map.put(source, new HashSet[Value])
	}
	def add[Key, Value](key: Key, value: Value, map: Map[Key, Set[Value]])
	{
		map.getOrElseUpdate(key, new HashSet[Value]) + value
	}
	
	def writeHashes(map: Map[Path, Array[Byte]], label: String, to: Path, log: Logger) =
	{
		val properties = new Properties
		for( (path, hash) <- map)
			properties.setProperty(path.relativePath, Hash.toHex(hash))
		PropertiesUtilities.write(properties, label, to, log)
	}
	
	def writeTestDefinitions(map: Map[Path, Set[TestDefinition]], label: String, to: Path, log: Logger) =
		write(map, label, (i: Iterable[TestDefinition]) => i.map(_.toString).mkString(File.pathSeparator), to, log)
	def writeStrings(map: Map[Path, Set[String]], label: String, to: Path, log: Logger) =
		write(map, label, (i: Iterable[String]) => i.mkString(File.pathSeparator), to, log)
	def writePaths(map: Map[Path, Set[Path]], label: String, to: Path, log: Logger) =
		write(map, label, Path.makeRelativeString, to, log)
	private def write[Value](map: Map[Path, Set[Value]], label: String,
		valuesToString: Iterable[Value] => String, to: Path, log: Logger): Option[String] =
	{
		val properties = new Properties
		for( (path, set) <- map)
			properties.setProperty(path.relativePath, valuesToString(set))
		PropertiesUtilities.write(properties, label, to, log)
	}
	def writeFilePaths(map: Map[File, Set[Path]], label: String, to: Path, log: Logger) =
	{
		val properties = new Properties
		for( (file, set) <- map)
			properties.setProperty(file.getCanonicalPath, Path.makeRelativeString(set))
		PropertiesUtilities.write(properties, label, to, log)
	}
	
	private def pathSetFromString(projectPath: Path)(s: String): Set[Path] =
		(new HashSet[Path]) ++ Path.splitString(projectPath, s)
	private def stringToSet[T](f: String => T)(s: String): Set[T] =
		(new HashSet[T]) ++ FileUtilities.pathSplit(s).map(_.trim).filter(_.length > 0).map(f)
	
	def loadHashes(map: Map[Path, Array[Byte]], from: Path, projectPath: Path, log: Logger) =
		load(map, Hash.fromHex, from, projectPath, log)
	def loadTestDefinitions(map: Map[Path, Set[TestDefinition]], from: Path, projectPath: Path, log: Logger) =
		loadStrings(map, t => TestParser.parse(t).fold(error, x => x), from, projectPath, log)
	def loadStrings(map: Map[Path, Set[String]], from: Path, projectPath: Path, log: Logger): Option[String] =
		loadStrings(map, x => x, from, projectPath, log)
	def loadStrings[T](map: Map[Path, Set[T]], f: String => T, from: Path, projectPath: Path, log: Logger): Option[String] =
		load(map, stringToSet[T](f)(_), from, projectPath, log)
	def loadPaths(map: Map[Path, Set[Path]], from: Path, projectPath: Path, log: Logger) =
		load(map, pathSetFromString(projectPath)(_), from, projectPath, log)
		
	private def load[Value](map: Map[Path, Value], stringToValue: String => Value, from: Path, projectPath: Path, log: Logger): Option[String] =
	{
		map.clear
		val properties = new Properties
		PropertiesUtilities.load(properties, from, log) orElse
		{
			for(name <- PropertiesUtilities.propertyNames(properties))
				map.put(Path.fromString(projectPath, name), stringToValue(properties.getProperty(name)))
			None
		}
	}
	def loadFilePaths(map: Map[File, Set[Path]], from: Path, projectPath: Path, log: Logger) =
	{
		map.clear
		val properties = new Properties
		PropertiesUtilities.load(properties, from, log) orElse
		{
			for(name <- PropertiesUtilities.propertyNames(properties))
				map.put(new File(name), pathSetFromString(projectPath)(properties.getProperty(name)))
			None
		}
	}
}