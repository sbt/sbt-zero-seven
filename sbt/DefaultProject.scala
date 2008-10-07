/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah, David MacIver
 */
package sbt

class DefaultProject(val info: ProjectInfo, val analysis: ProjectAnalysis) extends Project with ConsoleLogger
{
	def mainClass: Option[String] = None

  lazy val clean = cleanTask(outputPath, ClearAnalysis :: Nil);
  lazy val compile = compileTask(mainSources +++ testSources, compilePath, compileOptions, false);
  lazy val run = runTask(mainClass, compilePath +++ libraries, runOptions).dependsOn(compile);
  lazy val console = consoleTask(compilePath +++ libraries).dependsOn(compile);
  lazy val doc = scaladocTask(mainSources, mainDocPath, compilePath, documentOptions).dependsOn(compile);
  lazy val docTest = scaladocTask(testSources, testDocPath, compilePath, documentOptions).dependsOn(compile);
  lazy val test = testTask(compilePath +++ libraries, Nil).dependsOn(compile);
  lazy val `package` = packageTask(getClasses(mainSources) +++ mainResources, mainClass.map(MainClassOption(_)).toList).dependsOn(compile);
	lazy val packageTest = packageTask(getClasses(testSources) +++ testResources, JarName(defaultJarBaseName + "-test.jar") :: Nil).dependsOn(test);
	lazy val packageDocs =	packageTask(mainDocPath ##, JarName(defaultJarBaseName + "-docs.jar") :: Nil).dependsOn(doc);
  lazy val packageSrc = packageTask(allSources, JarName(defaultJarBaseName + "-src.jar") :: Nil)
  lazy val docAll = doc && docTest;
  lazy val packageAll = `package` && packageTest && packageSrc;
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
