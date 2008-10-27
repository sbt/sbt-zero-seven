/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

import FileUtilities._
import java.io.File
import java.util.jar.{Attributes, Manifest}

trait ScalaProject extends Project
{
	import ScalaProject._
	lazy val analysis: ProjectAnalysis =
	{
		val a = new ProjectAnalysis(info.builderPath / AnalysisDirectoryName, info.projectPath, this)
		for(errorMessage <- a.load())
			error(errorMessage)
		a
	}
	
	def getClasses(sources: PathFinder, outputDirectory: Path): PathFinder =
		Path.lazyPathFinder
		{
			val basePath = (outputDirectory ##)
			for(c <- analysis.getClasses(sources.get)) yield
				Path.relativize(basePath, c) match
				{
					case Some(relativized) => relativized
					case None => c
				}
		}
	
	def analysisCallback: AnalysisCallback =
		new BasicAnalysisCallback(info.projectPath, List(ScalaCheckPropertiesClassName), analysis)
		{
			def foundSubclass(sourcePath: Path, subclassName: String, superclassName: String, isModule: Boolean)
			{
				if(superclassName == ScalaCheckPropertiesClassName && isModule)
					analysis.addTest(sourcePath, subclassName)
			}
		}
	
	def isModified(source: Path) =
	{
		assert(source.asFile.exists, "Non-existing file " + source)
		analysis.getClasses(source) match
		{
			case None =>
			{
				debug("New file " + source)
				true
			}
			case Some(classes) =>
			{
				val sourceModificationTime = source.asFile.lastModified
				def isUpdated(p: Path) =
				{
					val f = p.asFile
					!f.exists || f.lastModified < sourceModificationTime
				}
				val modified = classes.find(isUpdated)
				if(modified.isDefined)
					debug("Modified class: " + modified.get)
				modified.isDefined
			}
		}
	}
	def conditionalAction(sources: PathFinder, classpath: PathFinder, execute: Iterable[Path] => Option[String]) =
		task
		{
			val result = execute(dirtySources(sources, classpath))
			info("  Post-analysis: " + analysis.allClasses.toSeq.length + " classes.")
			if(result.isEmpty)
				analysis.save()
			else
				analysis.load()
			result
		}
		
	private def dirtySources(sources: PathFinder, classpath: PathFinder): Iterable[Path] =
	{
		import scala.collection.mutable.HashSet
		
		val sourcesSnapshot = sources.get
		val removedSources = new HashSet[Path]
		removedSources ++= analysis.allSources
		removedSources --= sourcesSnapshot
		val removedCount = removedSources.size
		for(removed <- removedSources)
			analysis.removeDependent(removed)
		
		val unmodified = new HashSet[Path]
		val modified = new HashSet[Path]
		
		for(source <- sourcesSnapshot)
		{
			if(isModified(source))
			{
				debug("Source " + source + " directly modified.")
				modified += source
			}
			else
			{
				debug("Source " + source + " unmodified.")
				unmodified += source
			}
		}
		val directlyModifiedCount = modified.size
		
		val (classpathJars, classpathDirs) = ClasspathUtilities.buildSearchPaths(classpath.get)
		for((externalDependency, dependentSources) <- analysis.allExternalDependencies)
		{
			if(externalDependency.exists &&
				ClasspathUtilities.onClasspath(classpathJars, classpathDirs, externalDependency))
			{
				val dependencyLastModified = externalDependency.lastModified
				for(dependentSource <- dependentSources; classes <- analysis.getClasses(dependentSource))
				{
					classes.find(_.asFile.lastModified < dependencyLastModified) match
					{
						case Some(modifiedClass) => 
						{
							debug("Class " + modifiedClass + " older than external dependency " + externalDependency.getCanonicalPath)
							unmodified -= dependentSource
							modified += dependentSource
						}
						case None => ()
					}
				}
			}
			else
			{
				debug("External dependency " + externalDependency + " not on classpath")
				unmodified --= dependentSources
				modified ++= dependentSources
				analysis.removeExternalDependency(externalDependency)
			}
		}
		
		def markDependenciesModified(changed: Iterable[Path]): List[Path] =
		{
			var newChanges: List[Path] = Nil
			for(changedPath <- changed; dependencies <- analysis.removeDependencies(changedPath);
				dependsOnChanged <- dependencies)
			{
				unmodified -= dependsOnChanged
				modified += dependsOnChanged
				newChanges = dependsOnChanged :: newChanges
			}
			newChanges
		}
		def propagateChanges(changed: Iterable[Path])
		{
			val newChanges = markDependenciesModified(changed)
			if(newChanges.length > 0)
				propagateChanges(newChanges)
		}
		
		propagateChanges(modified ++ markDependenciesModified(removedSources))
		for(changed <- removedSources ++ modified)
			analysis.removeSource(changed)
		
		info("  Source analysis: " + directlyModifiedCount + " new/modifed, " +
			(modified.size-directlyModifiedCount) + " indirectly invalidated, " +
			removedCount + " removed.")
		
		modified
	}

	def errorTask(message: String) = task{ Some(message) }
	
	trait ActionOption extends NotNull
	
	case class CompileOption(val asString: String) extends ActionOption
	trait PackageOption extends ActionOption
	trait TestOption extends ActionOption
	trait CleanOption extends ActionOption
	object IncrementVersion extends ActionOption
	object ClearAnalysis extends CleanOption
	
	case class ExcludeTests(classNames: Iterable[String]) extends TestOption
	
	case class ManifestOption(m: Manifest) extends PackageOption
	{
		assert(m != null)
	}
	case class MainClassOption(mainClassName: String) extends PackageOption
	case object Recursive extends PackageOption
	
	val Deprecation = CompileOption("-deprecation")
	val ExplainTypes = CompileOption("-explaintypes")
	val Optimize = CompileOption("-optimise")
	val Verbose = CompileOption("-verbose")
	val Unchecked = CompileOption("-unchecked")
	val DisableWarnings = CompileOption("-nowarn")
	def target(target: Target.Value) = CompileOption("-target:" + target)
	object Target extends Enumeration
	{
		val Java1_5 = Value("jvm-1.5")
		val Java1_4 = Value("jvm-1.4")
		val Msil = Value("msil")
	}
	
	trait ScaladocOption extends ActionOption
	{
		def asList: List[String]
	}
	case class SimpleDocOption(optionValue: String) extends ScaladocOption
	{
		def asList = List(optionValue)
	}
	case class CompoundDocOption(label: String, value: String) extends ScaladocOption
	{
		def asList = List(label, value)
	}
	val LinkSource = SimpleDocOption("-linksource")
	val NoComment = SimpleDocOption("-nocomment")
	def access(access: Access.Value) = SimpleDocOption("-access:" + access)
	def documentBottom(bottomText: String) = CompoundDocOption("-bottom", bottomText)
	def documentCharset(charset: String) = CompoundDocOption("-charset", charset)
	def documentTitle(title: String) = CompoundDocOption("-doctitle", title)
	def documentFooter(footerText: String) = CompoundDocOption("-footer", footerText)
	def documentHeader(headerText: String) = CompoundDocOption("-header", headerText)
	def stylesheetFile(path: Path) = CompoundDocOption("-stylesheetfile", path.asFile.getAbsolutePath)
	def documentTop(topText: String) = CompoundDocOption("-top", topText)
	def windowTitle(title: String) = CompoundDocOption("-windowtitle", title)
	
	object Access extends Enumeration
	{
		val Public = Value("public")
		val Default = Value("protected")
		val Private = Value("private")
	}

	def consoleTask(classpath : PathFinder) = 
		task { Run.console(classpath.get, this) }

	def runTask(mainClass: Option[String], classpath: PathFinder, options: String*) =	
		task { Run(mainClass, classpath.get, options, this) }


	def cleanTask(paths: PathFinder, options: CleanOption*) =
		task {
			val pathClean = FileUtilities.clean(paths.get, this)
			if(options.elements.contains(ClearAnalysis))
			{
				analysis.clear()
				analysis.save()
			}
			pathClean
		}

	def testTask(classpath: PathFinder, options: TestOption*) = 
		task {
			import scala.collection.mutable.HashSet
			
			val tests = HashSet.empty[String] ++ analysis.allTests
			for(ExcludeTests(exclude) <- options)
				tests -- exclude
				
			if(tests.isEmpty)
			{
				info("No tests to run.")
				None
			}
			else
				ScalaCheckTests(classpath.get, tests, this)
		}

	def compileTask( sources: PathFinder, outputDirectory: Path, classpath: PathFinder, options: CompileOption*): Task =
		conditionalAction(sources, classpath,
			(dirtySources: Iterable[Path]) =>
			{
				val classpathString = Path.makeString(classpath.get)
				val id = AnalysisCallback.register(analysisCallback)
				val allOptions = (CompileOption("-Xplugin:" + sbtJar.getCanonicalPath) ::
					CompileOption("-P:sbt-analyzer:callback:" + id.toString) :: Nil) ++ options
				val r = Compile(dirtySources, classpathString, outputDirectory, allOptions.map(_.asString), this)
				AnalysisCallback.unregister(id)
				if(atLevel(Level.Debug))
				{
					/** This checks that the plugin accounted for all classes in the output directory.*/
					val classes = scala.collection.mutable.HashSet(analysis.allClasses.toSeq: _*)
					var missed = 0
					for(c <- (outputDirectory ** "*.class").get)
					{
						if(!classes.contains(c))
						{
							missed += 1
							debug("Missed class: " + c)
						}
					}
					debug("Total missed classes: " + missed)
				}
				r
			})
	def graphTask(outputDirectory: Path) = task { DotGraph(analysis, outputDirectory, this) }
	def scaladocTask(sources: PathFinder, outputDirectory: Path, classpath: PathFinder, options: ScaladocOption*) =
		task
		{
			val classpathString = Path.makeString(classpath.get)
			Scaladoc(sources.get, classpathString, outputDirectory, options.flatMap(_.asList), this)
		}

	def packageTask(sources: PathFinder, outputDirectory: Path, jarName: String, options: PackageOption*) =
		task
		{
			import scala.collection.jcl.Map
			/** Copies the mappings in a2 to a1, mutating a1. */
			def mergeAttributes(a1: Attributes, a2: Attributes)
			{
				for( (key, value) <- Map(a2))
					a1.put(key, value)
			}

			import scala.collection.mutable.ListBuffer
			val manifest = new Manifest
			var recursive = false
			for(option <- options)
			{
				option match
				{
					case ManifestOption(mergeManifest) => 
					{
						mergeAttributes(manifest.getMainAttributes, mergeManifest.getMainAttributes)
						val entryMap = Map(manifest.getEntries)
						for((key, value) <- Map(mergeManifest.getEntries))
						{
							entryMap.get(key) match
							{
								case Some(attributes) => mergeAttributes(attributes, value)
								case None => entryMap.put(key, value)
							}
						}
					}
					case Recursive => recursive = true
					case MainClassOption(mainClassName) => manifest.getMainAttributes.putValue(MainClassKey, mainClassName)
					case _ => warn("Ignored unknown package option " + option)
				}
			}
			
			val jarPath = outputDirectory / jarName
			FileUtilities.pack(sources.get, jarPath, manifest, recursive, this)
		}
}

trait ManagedScalaProject extends ScalaProject
{
	trait ManagedOption extends ActionOption
	object Synchronize extends ManagedOption
	object Validate extends ManagedOption
	final case class LibraryManager(m: Manager) extends ManagedOption
	
	def updateTask(outputPattern: String, managedDependencyPath: Path, options: ManagedOption*) =
		task
		{
			var synchronize = false
			var validate = false
			var manager: Manager = AutoDetectManager
			for(option <- options)
			{
				option match
				{
					case Synchronize => synchronize = true
					case Validate => validate = true
					case LibraryManager(m) => manager = m
					case _ => warn("Ignored unknown managed option " + option)
				}
			}
			try
			{
				ManageDependencies.update(info.projectPath, outputPattern, managedDependencyPath, manager,
					validate, synchronize, this)
			}
			catch
			{
				case e: NoClassDefFoundError =>
					trace(e)
					Some("Apache Ivy is required for dependency management (" + e.toString + ")")
			}
		}
}
object ScalaProject
{
	val AnalysisDirectoryName = "analysis"
	val MainClassKey = "Main-Class"
	val ScalaCheckPropertiesClassName = "org.scalacheck.Properties" 
}