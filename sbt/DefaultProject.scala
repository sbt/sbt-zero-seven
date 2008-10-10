/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

class DefaultProject(val info: ProjectInfo, val analysis: ProjectAnalysis) extends Project with ConsoleLogger
{
	import DefaultProject._
	def mainClass: Option[String] = None

	lazy val clean = cleanTask(outputPath, ClearAnalysis :: Nil) describedAs CleanDescription
	lazy val compile = compileTask(mainSources +++ testSources, compilePath, compileOptions, false) describedAs CompileDescription
	lazy val run = runTask(mainClass, compilePath +++ libraries, runOptions).dependsOn(compile) describedAs RunDescription
	lazy val console = consoleTask(compilePath +++ libraries).dependsOn(compile) describedAs ConsoleDescription
	lazy val doc = scaladocTask(mainSources, mainDocPath, compilePath, documentOptions).dependsOn(compile) describedAs DocDescription
	lazy val docTest = scaladocTask(testSources, testDocPath, compilePath, documentOptions).dependsOn(compile) describedAs TestDocDescription
	lazy val test = testTask(compilePath +++ libraries, Nil).dependsOn(compile) describedAs TestDescription
	lazy val `package` = packageTask(getClasses(mainSources) +++ mainResources, mainClass.map(MainClassOption(_)).toList).dependsOn(compile) describedAs PackageDescription
	lazy val packageTest = packageTask(getClasses(testSources) +++ testResources, JarName(defaultJarBaseName + "-test.jar") :: Nil).dependsOn(test) describedAs TestPackageDescription
	lazy val packageDocs = packageTask(mainDocPath ##, JarName(defaultJarBaseName + "-docs.jar") :: Nil).dependsOn(doc) describedAs DocPackageDescription
	lazy val packageSrc = packageTask(allSources, JarName(defaultJarBaseName + "-src.jar") :: Nil) describedAs SourcePackageDescription
	lazy val docAll = (doc && docTest) describedAs DocAllDescription
	lazy val packageAll = (`package` && packageTest && packageSrc) describedAs PackageAllDescription
	lazy val release = clean && compile && test && packageAll && doc 

	def allSources = (sourcePath ##) ** "*.scala" - ".svn"
	def mainSources = mainScalaSourcePath ** "*.scala" - ".svn"
	def testSources = testScalaSourcePath ** "*.scala" - ".svn"
	def mainResources = (mainResourcesPath ##) -- ".svn"
	def testResources = (testResourcesPath ##) -- ".svn"
	def libraries = dependencyPath ** "*.jar" - ".svn"
	
	import Project._
	
	def runOptions: Seq[String] = Nil
	def compileOptions = Deprecation :: Nil
	def documentOptions: Seq[ScaladocOption] =
		LinkSource ::
		documentTitle(info.name + " " + info.currentVersion + " API") ::
		windowTitle(info.name + " " + info.currentVersion + " API") ::
		Nil
}

object DefaultProject
{
	val CleanDescription =
		"Deletes all generated files (the target directory and the metadata/analysis directory)."
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
}