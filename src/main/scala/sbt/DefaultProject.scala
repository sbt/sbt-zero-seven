/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

/** The default project when no project is explicitly configured and the common base class for
* configuring a project.*/
class DefaultProject(val info: ProjectInfo) extends BasicScalaProject

abstract class BasicScalaProject extends ManagedScalaProject with BasicProjectPaths with ReflectiveManagedProject
{
	import BasicScalaProject._
	/** The class to be run by the 'run' action.
	* See http://code.google.com/p/simple-build-tool/wiki/RunningProjectCode for details.*/
	def mainClass: Option[String] = None
	def dependencies = info.dependencies

	val mainCompileConditional = new CompileConditional(mainCompileConfiguration)
	val testCompileConditional = new CompileConditional(testCompileConfiguration)

	def defaultExcludes = ".svn" | ".cvs"
	
	def allSources =
	{
		val sourceDirs = (mainScalaSourcePath +++ mainResourcesPath +++ testScalaSourcePath +++ testResourcesPath)
		descendents(sourceDirs, "*")
	}
	/** Short for parent.descendentsExcept(include, defaultExcludes)*/
	def descendents(parent: PathFinder, include: NameFilter) = parent.descendentsExcept(include, defaultExcludes)
	
	def mainSources = descendents(mainScalaSourcePath, "*.scala")
	def testSources = descendents(testScalaSourcePath, "*.scala")
	def mainResources = descendents(mainResourcesPath ##, "*")
	def testResources = descendents(testResourcesPath ##, "*")
	
	def mainClasses = (mainCompilePath ##) ** "*.class"
	def testClasses = (testCompilePath ##) ** "*.class"
	
	import Project._
	
	def normalizedName = name.toLowerCase.replaceAll("""\s+""", "-")
	def projectID: ModuleID =
	{
		normalizedName % normalizedName % version.toString
	}
	def excludeIDs = projectID :: Nil
	def manager = SimpleManager(ivyXML, true, projectID, repositories, libraryDependencies.toList: _*)
	
	def outputPattern = "[conf]/[artifact](-[revision]).[ext]"
	def ivyXML: scala.xml.NodeSeq = scala.xml.NodeSeq.Empty
	def baseUpdateOptions = Validate :: Synchronize :: QuietUpdate :: AddScalaToolsReleases :: Nil
	/** The options provided to the 'update' action.*/
	def updateOptions: Seq[ManagedOption] =
	{
		val m = manager
		if(m.dependencies.isEmpty && m.resolvers.isEmpty && ivyXML.isEmpty)
			baseUpdateOptions
		else
			LibraryManager(m) :: baseUpdateOptions
	}
	/** The options provided to the 'compile' action.*/
	def compileOptions: Seq[CompileOption] = Deprecation :: Nil
	/** The options provided to the 'test-compile' action, defaulting to those for the 'compile' action.*/
	def testCompileOptions: Seq[CompileOption] = compileOptions
	
	/** The options provided to the 'run' action.*/
	def runOptions: Seq[String] = Nil
	/** The options provided to the 'doc' and 'docTest' actions.*/
	def documentOptions: Seq[ScaladocOption] =
		LinkSource ::
		documentTitle(name + " " + version + " API") ::
		windowTitle(name + " " + version + " API") ::
		Nil
	/** The options provided to the 'test' action.*/
	def testOptions: Seq[TestOption] = Nil
	def cleanOptions: Seq[CleanOption] =
		ClearAnalysis(mainCompileConditional.analysis) ::
		ClearAnalysis(testCompileConditional.analysis) ::
		Nil
	
	private def directoriesToCreate: List[Path] =
		dependencyPath ::
		mainScalaSourcePath ::
		mainResourcesPath ::
		testScalaSourcePath ::
		testResourcesPath ::
		Nil
	
	override final def initializeDirectories()
	{
		FileUtilities.createDirectories(directoriesToCreate.map(_.asFile), log) match
		{
			case Some(errorMessage) => log.error("Could not initialize directory structure: " + errorMessage)
			case None => log.success("Successfully initialized directory structure.")
		}
	}
	def consoleConfiguration = Configurations.Test
	
	def docClasspath = fullClasspath(Configurations.Compile)
	def compileClasspath = fullClasspath(Configurations.Compile)
	def testClasspath = fullClasspath(Configurations.Test, true)
	def runClasspath = fullClasspath(Configurations.Runtime)
	def consoleClasspath = fullClasspath(consoleConfiguration)
	
	def managedClasspath(config: String): PathFinder =
	{
		val configDirectory = managedDependencyPath / config
		val useDirectory =
			if(configDirectory.asFile.exists)
				configDirectory
			else
				managedDependencyPath / Configurations.Default
		descendents(useDirectory, "*.jar")
	}
	def unmanagedClasspath: PathFinder = descendents(dependencyPath, "*.jar")
	def projectClasspath(config: String, includeTestCompilePath: Boolean) =
	{
		val base = mainCompilePath +++ unmanagedClasspath +++ managedClasspath(config)
		if(includeTestCompilePath)
			testCompilePath +++ base
		else
			base
	}
	
	// includes classpaths of dependencies
	def fullClasspath(config: String): PathFinder = fullClasspath(config, false)
	def fullClasspath(config: String, includeTestCompilePath: Boolean): PathFinder =
		Path.lazyPathFinder
		{
			val set = new scala.collection.jcl.LinkedHashSet[Path]
			for(project <- topologicalSort)
			{
				project match
				{
					case sp: BasicScalaProject => set ++= sp.projectClasspath(config, includeTestCompilePath).get
					case _ => ()
				}
			}
			set.toList
		}
		
	/** The list of test frameworks to use for testing.  Note that adding frameworks to this list
	* for an active project currently requires an explicit 'clean'. */
	def testFrameworks: Iterable[TestFramework] = ScalaCheckFramework :: SpecsFramework :: ScalaTestFramework :: Nil
		
	def mainLabel = "main"
	def testLabel = "test"
		
	def mainCompileConfiguration = new MainCompileConfig
	def testCompileConfiguration = new TestCompileConfig
	abstract class BaseCompileConfig extends CompileConfiguration
	{
		def log = BasicScalaProject.this.log
		def projectPath = info.projectPath
	}
	class MainCompileConfig extends BaseCompileConfig
	{
		def label = mainLabel
		def sources = mainSources
		def outputDirectory = mainCompilePath
		def classpath = compileClasspath
		def analysisPath = mainAnalysisPath
		def testDefinitionClassNames = Nil
		def options = compileOptions.map(_.asString)
	}
	class TestCompileConfig extends BaseCompileConfig
	{
		def label = testLabel
		def sources = testSources
		def outputDirectory = testCompilePath
		def classpath = testClasspath
		def analysisPath = testAnalysisPath
		def testDefinitionClassNames = testFrameworks.map(_.testSuperClassName)
		def options = testCompileOptions.map(_.asString)
	}
	
	lazy val compile = task { mainCompileConditional.run } describedAs MainCompileDescription
	lazy val testCompile = task { testCompileConditional.run } dependsOn compile describedAs TestCompileDescription
	lazy val clean = cleanTask(outputPath, cleanOptions) describedAs CleanDescription
	lazy val run = runTask(mainClass, runClasspath, runOptions).dependsOn(compile) describedAs RunDescription
	lazy val console = consoleTask(consoleClasspath).dependsOn(testCompile) describedAs ConsoleDescription
	lazy val doc = scaladocTask(mainLabel, mainSources, mainDocPath, docClasspath, documentOptions).dependsOn(compile) describedAs DocDescription
	lazy val docTest = scaladocTask(testLabel, testSources, testDocPath, docClasspath, documentOptions).dependsOn(testCompile) describedAs TestDocDescription
	lazy val test = testTask(testFrameworks, testClasspath, testCompileConditional.analysis, testOptions).dependsOn(testCompile) describedAs TestDescription
	lazy val `package` = packageTask(mainClasses +++ mainResources, outputPath, defaultJarName, mainClass.map(MainClassOption(_)).toList).dependsOn(compile) describedAs PackageDescription
	lazy val packageTest = packageTask(testClasses +++ testResources, outputPath, defaultJarBaseName + "-test.jar").dependsOn(testCompile) describedAs TestPackageDescription
	lazy val packageDocs = packageTask(mainDocPath ##, outputPath, defaultJarBaseName + "-docs.jar", Recursive).dependsOn(doc) describedAs DocPackageDescription
	lazy val packageSrc = packageTask(allSources, outputPath, defaultJarBaseName + "-src.jar") describedAs SourcePackageDescription
	lazy val docAll = (doc && docTest) describedAs DocAllDescription
	lazy val packageAll = (`package` && packageTest && packageSrc && packageDocs) describedAs PackageAllDescription
	lazy val graph = graphTask(graphPath, mainCompileConditional.analysis).dependsOn(compile)
	lazy val update = updateTask(outputPattern, managedDependencyPath, updateOptions) describedAs UpdateDescription
	lazy val cleanLib = cleanLibTask(managedDependencyPath) describedAs CleanLibDescription
	lazy val cleanCache = cleanCacheTask(managedDependencyPath, updateOptions) describedAs CleanCacheDescription
	lazy val incrementVersion = task { incrementVersionNumber(); None } describedAs IncrementVersionDescription
	lazy val release = (test && packageAll && incrementVersion) describedAs ReleaseDescription
	
	import StringUtilities.nonEmpty
	implicit def toGroupID(groupID: String): GroupID =
	{
		nonEmpty(groupID, "Group ID")
		GroupID(groupID)
	}
	implicit def toRepositoryName(name: String): RepositoryName =
	{
		nonEmpty(name, "Repository name")
		RepositoryName(name)
	}
}

object BasicScalaProject
{
	val CleanDescription =
		"Deletes all generated files (the target directory)."
	val MainCompileDescription =
		"Compiles main sources."
	val TestCompileDescription =
		"Compiles test sources."
	val TestDescription =
		"Runs all tests detected during compilation."
	val DocDescription =
		"Generates API documentation for main Scala source files using scaladoc."
	val TestDocDescription =
		"Generates API documentation for test Scala source files using scaladoc."
	val RunDescription =
		"Runs the main class specified for the project or starts the console if main class is not specified."
	val ConsoleDescription =
		"Starts the Scala interpreter with the project classes on the classpath."
	val PackageDescription =
		"Creates a jar file containing main classes and resources."
	val TestPackageDescription =
		"Creates a jar file containing test classes and resources."
	val DocPackageDescription =
		"Creates a jar file containing generated API documentation."
	val SourcePackageDescription =
		"Creates a jar file containing all source files."
	val PackageAllDescription =
		"Executes all package tasks."
	val DocAllDescription =
		"Generates both main and test documentation."
	val UpdateDescription =
		"Resolves and retrieves automatically managed dependencies."
	val CleanLibDescription =
		"Deletes the managed library directory."
	val CleanCacheDescription =
		"Deletes the cache of artifacts downloaded for automatically managed dependencies."
	val IncrementVersionDescription =
		"Increments the micro part of the version (the third number) by one. (This is only valid for versions of the form #.#.#.*)"
	val ReleaseDescription =
		"Compiles, tests, generates documentation, packages, and increments the version."
		
	val ScalaCheckPropertiesClassName = "org.scalacheck.Properties" 
}
trait BasicProjectPaths extends Project
{
	import BasicProjectPaths._
	
	//////////// Paths ///////////
	
	def defaultJarBaseName = name + "-" + version.toString
	def defaultJarName = defaultJarBaseName + ".jar"
	
	def outputDirectoryName = DefaultOutputDirectoryName
	def sourceDirectoryName = DefaultSourceDirectoryName
	def mainDirectoryName = DefaultMainDirectoryName
	def scalaDirectoryName = DefaultScalaDirectoryName
	def resourcesDirectoryName = DefaultResourcesDirectoryName
	def testDirectoryName = DefaultTestDirectoryName
	def mainCompileDirectoryName = DefaultMainCompileDirectoryName
	def testCompileDirectoryName = DefaultTestCompileDirectoryName
	def dependencyDirectoryName = DefaultDependencyDirectoryName
	def docDirectoryName = DefaultDocDirectoryName
	def apiDirectoryName = DefaultAPIDirectoryName
	def graphDirectoryName = DefaultGraphDirectoryName
	def managedDirectoryName = DefaultManagedDirectoryName
	def mainAnalysisDirectoryName = DefaultMainAnalysisDirectoryName
	def testAnalysisDirectoryName = DefaultTestAnalysisDirectoryName
	
	def dependencyPath = path(dependencyDirectoryName)
	def managedDependencyPath = path(managedDirectoryName)
	def outputPath = path(outputDirectoryName)
	def sourcePath = path(sourceDirectoryName)
	
	def mainSourcePath = sourcePath / mainDirectoryName
	def mainScalaSourcePath = mainSourcePath / scalaDirectoryName
	def mainResourcesPath = mainSourcePath / resourcesDirectoryName
	def mainDocPath = docPath / mainDirectoryName / apiDirectoryName
	def mainCompilePath = outputPath / mainCompileDirectoryName
	def mainAnalysisPath = outputPath / mainAnalysisDirectoryName
	
	def testSourcePath = sourcePath / testDirectoryName
	def testScalaSourcePath = testSourcePath / scalaDirectoryName
	def testResourcesPath = testSourcePath / resourcesDirectoryName
	def testDocPath = docPath / testDirectoryName / apiDirectoryName
	def testCompilePath = outputPath / testCompileDirectoryName
	def testAnalysisPath = outputPath / testAnalysisDirectoryName
	
	def docPath = outputPath / docDirectoryName
	def graphPath = outputPath / graphDirectoryName
	
	/** The directories to which a project writes are listed here and is used
	* to check a project and its dependencies for collisions.*/
	override def outputDirectories = outputPath :: managedDependencyPath :: Nil
}
object BasicProjectPaths
{
	val DefaultSourceDirectoryName = "src"
	val DefaultOutputDirectoryName = "target"
	val DefaultMainCompileDirectoryName = "classes"
	val DefaultTestCompileDirectoryName = "test-classes"
	val DefaultDocDirectoryName = "doc"
	val DefaultAPIDirectoryName = "api"
	val DefaultGraphDirectoryName = "graph"
	val DefaultManagedDirectoryName = "lib_managed"
	val DefaultMainAnalysisDirectoryName = "analysis"
	val DefaultTestAnalysisDirectoryName = "test-analysis"
	
	val DefaultMainDirectoryName = "main"
	val DefaultScalaDirectoryName = "scala"
	val DefaultResourcesDirectoryName = "resources"
	val DefaultTestDirectoryName = "test"
	val DefaultDependencyDirectoryName = "lib"
}
object StringUtilities
{
	def nonEmpty(s: String, label: String)
	{
		require(s.length > 0, label + " cannot be empty.")
	}
}
import StringUtilities.nonEmpty
final case class GroupID(groupID: String) extends NotNull
{
	def % (artifactID: String) =
	{
		nonEmpty(artifactID, "Artifact ID")
		GroupArtifactID(groupID, artifactID)
	}
}
final case class GroupArtifactID(groupID: String, artifactID: String) extends NotNull
{
	def % (revision: String): ModuleID =
	{
		nonEmpty(revision, "Revision")
		ModuleID(groupID, artifactID, revision)
	}
}
final case class RepositoryName(name: String) extends NotNull
{
	def at (location: String) =
	{
		nonEmpty(location, "Repository location")
		MavenRepository(name, location)
	}
}