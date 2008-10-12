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
		val a = new ProjectAnalysis
		for(errorMessage <- a.load(info, this))
			error(errorMessage)
		a
	}
	
	def getClasses(sources: PathFinder): PathFinder =
		Path.lazyPathFinder
		{
			val basePath = (compilePath ##)
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
	def conditionalAction(sources : PathFinder, execute : Iterable[Path] => Option[String]) =
		task{
			import scala.collection.mutable.HashSet
			val sourcesSnapshot = sources.get
			val dirtySources: Iterable[Path] =
			{
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
					analysis.removeSource(changed, this)
				
				info("  Source analysis: " + directlyModifiedCount + " new/modifed, " +
					(modified.size-directlyModifiedCount) + " indirectly invalidated, " +
					removedCount + " removed.")
				
				modified
			}
			
			val result = execute(dirtySources)
			info("  Post-analysis: " + analysis.allClasses.toSeq.length + " classes.")
			if(result.isEmpty)
				analysis.save(info, this)
			else
				analysis.load(info, this)
			result
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
	case class JarName(name: String) extends PackageOption
	{
		Path.checkComponent(name)
	}
	case class OutputDirectory(path: Path) extends PackageOption
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

	def runTask(mainClass : Option[String], classpath : PathFinder, options : Seq[String]) =	
		task { Run(mainClass, classpath.get, options, this) }


	def cleanTask(paths: PathFinder, options: Iterable[CleanOption]) =
		task {
			val pathClean = FileUtilities.clean(paths.get, this)
			if(options.elements.contains(ClearAnalysis))
			{
				analysis.clear
				analysis.save(info, this)
			}
			pathClean
		}

	def testTask(classpath: PathFinder, options: Iterable[TestOption]) = 
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

	def compileTask( sources: PathFinder, outputDirectory: Path, options: Iterable[CompileOption], includeSbtInClasspath: Boolean): Task =
		conditionalAction(sources, 
			(dirtySources: Iterable[Path]) =>
			{
				val classpathString = Path.makeString(fullClasspath.get) +
					(if(includeSbtInClasspath) File.pathSeparator + sbtJar else "")
				val id = AnalysisCallback.register(analysisCallback)
				val allOptions = (CompileOption("-Xplugin:" + sbtJar.getCanonicalPath) ::
					CompileOption("-P:sbt-analyzer:callback:" + id.toString) :: Nil) ++ options
				val r = Compile(dirtySources, classpathString, outputDirectory, allOptions.map(_.asString), this)
				AnalysisCallback.unregister(id)
				if(atLevel(Level.Debug))
				{
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
	
	def scaladocTask(sources: PathFinder, outputDirectory: Path, compiledPaths: PathFinder, options: Iterable[ScaladocOption]) =
		task
		{
			val classpathString = Path.makeString(fullClasspath.get)
			Scaladoc(sources.get, classpathString, outputDirectory, options.flatMap(_.asList), this)
		}

	def packageTask(sources: PathFinder, options: Iterable[PackageOption]) =
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
			var jarName: Option[String] = None
			var outputDirectory: Option[Path] = None
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
					case JarName(name) =>
						if(jarName.isEmpty)
							jarName = Some(name)
						else
							warn("Ignored duplicate jar name option " + name)
					case OutputDirectory(path) =>
						if(outputDirectory.isEmpty)
							outputDirectory = Some(path)
						else
							warn("Ignored duplicate output directory option " + path.toString)
					case _ => warn("Ignored unknown package option " + option)
				}
			}
			
			val jarPath = outputDirectory.getOrElse(path(DefaultOutputDirectoryName)) / jarName.getOrElse(defaultJarName)
			FileUtilities.pack(sources.get, jarPath, manifest, recursive, this)
		}
	
	override def initializeDirectories()
	{
		val toCreate =
			dependencyPath ::
			sourcePath ::
			mainSourcePath ::
			mainScalaSourcePath ::
			mainResourcesPath ::
			testSourcePath ::
			testScalaSourcePath ::
			testResourcesPath ::
			Nil
		
		FileUtilities.createDirectories(toCreate.map(_.asFile), this) match
		{
			case Some(errorMessage) => error("Could not initialize directory structure: " + errorMessage)
			case None => success("Successfully initialized directory structure.")
		}
	}
	
	def libraries = dependencyPath ** "*.jar" - ".svn"
	def projectClasspath: PathFinder = compilePath +++ libraries
	// includes classpaths of dependencies
	def fullClasspath: PathFinder =
		Path.lazyPathFinder
		{
			val set = new scala.collection.jcl.LinkedHashSet[Path]
			for(project <- topologicalSort.reverse)
			{
				project match
				{
					case sp: ScalaProject => set ++= sp.projectClasspath.get
					case _ => ()
				}
			}
			set.toList
		}
	
	def defaultJarBaseName = info.name + "-" + info.currentVersion.toString
	def defaultJarName = defaultJarBaseName + ".jar"
	
	def outputDirectoryName = DefaultOutputDirectoryName
	def sourceDirectoryName = DefaultSourceDirectoryName
	def mainDirectoryName = DefaultMainDirectoryName
	def scalaDirectoryName = DefaultScalaDirectoryName
	def resourcesDirectoryName = DefaultResourcesDirectoryName
	def testDirectoryName = DefaultTestDirectoryName
	def compileDirectoryName = DefaultCompileDirectoryName
	def dependencyDirectoryName = DefaultDependencyDirectoryName
	def docDirectoryName = DefaultDocDirectoryName
	def apiDirectoryName = DefaultAPIDirectoryName
	
	def dependencyPath = path(dependencyDirectoryName)
	def outputPath = path(outputDirectoryName)
	def sourcePath = path(sourceDirectoryName)
	
	def mainSourcePath = sourcePath / mainDirectoryName
	def mainScalaSourcePath = mainSourcePath / scalaDirectoryName
	def mainResourcesPath = mainSourcePath / resourcesDirectoryName
	def mainDocPath = docPath / mainDirectoryName / apiDirectoryName
	
	def testSourcePath = sourcePath / testDirectoryName
	def testScalaSourcePath = testSourcePath / scalaDirectoryName
	def testResourcesPath = testSourcePath / resourcesDirectoryName
	def testDocPath = docPath / testDirectoryName / apiDirectoryName
	
	def compilePath = outputPath / compileDirectoryName
	def docPath = outputPath / docDirectoryName
}

object ScalaProject
{
	val DefaultSourceDirectoryName = "src"
	val DefaultOutputDirectoryName = "target"
	val DefaultCompileDirectoryName = "classes"
	val DefaultDocDirectoryName = "doc"
	val DefaultAPIDirectoryName = "api"
	
	val DefaultMainDirectoryName = "main"
	val DefaultScalaDirectoryName = "scala"
	val DefaultResourcesDirectoryName = "resources"
	val DefaultTestDirectoryName = "test"
	val DefaultDependencyDirectoryName = "lib"
	
	val MainClassKey = "Main-Class"
	
	val ScalaCheckPropertiesClassName = "org.scalacheck.Properties"
}