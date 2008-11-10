/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

class DefaultProject(val info: ProjectInfo, val dependencies: Iterable[Project]) extends BasicScalaProject
{
	def this(i: ProjectInfo) = this(i, Nil)
}

abstract class BasicScalaProject extends ManagedScalaProject with BasicProjectPaths with ReflectiveProject
{
	import BasicScalaProject._
	def mainClass: Option[String] = None

	val compileConditional = new CompileConditional(compileConfiguration)

	def allSources = (sourcePath ##) ** defaultIncludeAll
	def mainSources = mainScalaSourcePath ** defaultIncludeAll * "*.scala"
	def testSources = testScalaSourcePath ** defaultIncludeAll * "*.scala"
	def mainResources = (mainResourcesPath ##) ** defaultIncludeAll
	def testResources = (testResourcesPath ##) ** defaultIncludeAll
	
	def defaultExcludes = ".svn" | ".cvs"
	def defaultIncludeAll = -defaultExcludes
	
	import Project._
	
	def compileOptions: Seq[CompileOption] = Deprecation :: Nil
	def runOptions: Seq[String] = Nil
	def documentOptions: Seq[ScaladocOption] =
		LinkSource ::
		documentTitle(info.name + " " + info.currentVersion + " API") ::
		windowTitle(info.name + " " + info.currentVersion + " API") ::
		Nil
	def updateOptions: Seq[ManagedOption] = Validate :: Synchronize :: QuietUpdate :: Nil
	def testOptions: Seq[TestOption] = Nil
	
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
	def consoleConfiguration = Configurations.Runtime
	
	def docClasspath = fullClasspath(Configurations.Compile)
	def compileClasspath = fullClasspath(Configurations.Compile)
	def testClasspath = fullClasspath(Configurations.Test)
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
		useDirectory ** defaultIncludeAll * "*.jar"
	}
	def unmanagedClasspath: PathFinder =
		(dependencyPath * "*.jar") +++ ((dependencyPath * -managedDirectoryName) ** defaultIncludeAll * "*.jar")
	def projectClasspath(config: String) = compilePath +++ unmanagedClasspath +++ managedClasspath(config)
	
	// includes classpaths of dependencies
	def fullClasspath(config: String): PathFinder =
		Path.lazyPathFinder
		{
			val set = new scala.collection.jcl.LinkedHashSet[Path]
			for(project <- topologicalSort)
			{
				project match
				{
					case sp: BasicScalaProject => set ++= sp.projectClasspath(config).get
					case _ => ()
				}
			}
			set.toList
		}
		
	def testFrameworks: Iterable[TestFramework] = ScalaCheckFramework :: SpecsFramework :: ScalaTestFramework :: Nil
		
	def compileConfiguration = new DefaultCompileConfig
	class DefaultCompileConfig extends CompileConfiguration
	{
		def sources = mainSources +++ testSources
		def outputDirectory = compilePath
		def classpath = compileClasspath
		def analysisPath = BasicScalaProject.this.analysisPath
		def projectPath = info.projectPath
		def testDefinitionClassNames = testFrameworks.map(_.testSuperClassName)
		def log = BasicScalaProject.this.log
		def options = compileOptions.map(_.asString)
	}
	
	import compileConditional.analysis
	def getClasses(sources: PathFinder): PathFinder = analysis.getClasses(sources, compilePath)
	
	lazy val compile = task { compileConditional.run } describedAs CompileDescription
	lazy val clean = cleanTask(outputPath, ClearAnalysis(analysis)) describedAs CleanDescription
	lazy val run = runTask(mainClass, runClasspath, runOptions: _*).dependsOn(compile) describedAs RunDescription
	lazy val console = consoleTask(consoleClasspath).dependsOn(compile) describedAs ConsoleDescription
	lazy val doc = scaladocTask(mainSources, mainDocPath, docClasspath, documentOptions: _*).dependsOn(compile) describedAs DocDescription
	lazy val docTest = scaladocTask(testSources, testDocPath, docClasspath, documentOptions: _*).dependsOn(compile) describedAs TestDocDescription
	lazy val test = testTask(testFrameworks, testClasspath, analysis, testOptions: _*).dependsOn(compile) describedAs TestDescription
	lazy val `package` = packageTask(getClasses(mainSources) +++ mainResources, outputPath, defaultJarName, mainClass.map(MainClassOption(_)).toList: _*).dependsOn(compile) describedAs PackageDescription
	lazy val packageTest = packageTask(getClasses(testSources) +++ testResources, outputPath, defaultJarBaseName + "-test.jar").dependsOn(compile) describedAs TestPackageDescription
	lazy val packageDocs = packageTask(mainDocPath ##, outputPath, defaultJarBaseName + "-docs.jar", Recursive).dependsOn(doc) describedAs DocPackageDescription
	lazy val packageSrc = packageTask(allSources, outputPath, defaultJarBaseName + "-src.jar") describedAs SourcePackageDescription
	lazy val docAll = (doc && docTest) describedAs DocAllDescription
	lazy val packageAll = (`package` && packageTest && packageSrc && packageDocs) describedAs PackageAllDescription
	lazy val release = clean && compile && test && packageAll
	lazy val graph = graphTask(graphPath, analysis).dependsOn(compile)
	lazy val update = updateTask("[conf]/[artifact](-[revision]).[ext]", managedDependencyPath, updateOptions: _*)
	lazy val cleanLib = cleanLibTask(managedDependencyPath)
}

object BasicScalaProject
{
	val CleanDescription =
		"Deletes all generated files (the target directory and the analysis directory)."
	val CompileDescription =
		"Compiles all sources."
	val TestDescription =
		"Runs all tests detected during compilation."
	val DocDescription =
		"Generates API documentation for main Scala source files using scaladoc."
	val TestDocDescription =
		"Generates API documentation for test Scala source files using scaladoc."
	val RunDescription =
		"Runs the main class specified for the project or starts the console if main class is undefined."
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
		
	val ScalaCheckPropertiesClassName = "org.scalacheck.Properties" 
}
trait BasicProjectPaths extends Project
{
	import BasicProjectPaths._
	
	//////////// Paths ///////////
		
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
	def graphDirectoryName = DefaultGraphDirectoryName
	def managedDirectoryName = DefaultManagedDirectoryName
	def analysisDirectoryName = DefaultAnalysisDirectoryName
	
	def dependencyPath = path(dependencyDirectoryName)
	final def managedDependencyPath = dependencyPath / managedDirectoryName
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
	def graphPath = outputPath / graphDirectoryName
	def analysisPath = outputPath / analysisDirectoryName
	
	/** The directories to which a project writes are listed here and is used
	* to check a project and its dependencies for collisions.*/
	override def outputDirectories = outputPath :: managedDependencyPath :: Nil
}
object BasicProjectPaths
{
	val DefaultSourceDirectoryName = "src"
	val DefaultOutputDirectoryName = "target"
	val DefaultCompileDirectoryName = "classes"
	val DefaultDocDirectoryName = "doc"
	val DefaultAPIDirectoryName = "api"
	val DefaultGraphDirectoryName = "graph"
	val DefaultManagedDirectoryName = "managed"
	val DefaultAnalysisDirectoryName = "analysis"
	
	val DefaultMainDirectoryName = "main"
	val DefaultScalaDirectoryName = "scala"
	val DefaultResourcesDirectoryName = "resources"
	val DefaultTestDirectoryName = "test"
	val DefaultDependencyDirectoryName = "lib"
}