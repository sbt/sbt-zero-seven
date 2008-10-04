package sbt

class DefaultProject(val info: ProjectInfo, val analysis: ProjectAnalysis) extends Project with ConsoleLogger
{
	def actions = defaultActions
	def mainClass: Option[String] = None
	
	def defaultActions =
		scala.collection.immutable.TreeMap(
			"clean" -> { clean },
			"compile" -> { compile },
			"console" -> { compile && console },
			"doc" -> { compile && doc },
			"doc-test" -> { compile && docTest },
			"doc-all" -> { compile && doc && docTest },
			"package" -> { compile && packageMain },
			"package-test" -> { compile && packageTest },
			"package-src" -> { compile && packageSource },
			"package-docs" -> { compile && packageDocs },
			"package-all" -> { compile && packageAll },
			"test" -> { compile && test },
			"release" -> { clean && compile && test && packageAll && doc },
			"run" -> { compile && run }
		)
	
	def run = RunAction(mainClass, compilePath +++ libraries, runOptions)
	def console = ConsoleAction(compilePath +++ libraries)
	def clean = CleanAction("target", ClearAnalysis :: Nil)
	
	def allSources = (sourcePath ##) ** "*.scala" - ".svn"
	def mainSources = mainScalaSourcePath ** "*.scala" - ".svn"
	def testSources = testScalaSourcePath ** "*.scala" - ".svn"
	def mainResources = (mainResourcesPath ##) -- ".svn"
	def testResources = (testResourcesPath ##) -- ".svn"
	def libraries = dependencyPath ** "*.jar" - ".svn"
	
	def compile = CompileAction("compile", mainSources +++ testSources, compilePath, compileOptions)
	def doc = ScaladocAction("doc", mainSources, mainDocPath, compilePath, documentOptions)
	def docTest = ScaladocAction("doc-test", testSources, testDocPath, compilePath, documentOptions)
	
	def test = TestAction(compilePath +++ libraries, Nil)
	
	import Project._
	def packageMain: Action =
	{
		val sources = getClasses(mainSources) +++ mainResources
		PackageAction(sources, mainClass.map(MainClassOption(_)).toList)
	}
	def packageTest: Action =
	{
		val sources = getClasses(testSources) +++ testResources
		PackageAction(sources, JarName(defaultJarBaseName + "-test.jar") :: Nil)
	}
	def packageDocs: Action =
		PackageAction(mainDocPath ##, JarName(defaultJarBaseName + "-docs.jar") :: Nil)
	def packageSource: Action =
		PackageAction(allSources, JarName(defaultJarBaseName + "-src.jar") :: Nil)
	def packageAll =
		packageMain && packageTest && packageSource && packageDocs
	
	def runOptions: Seq[String] = Nil
	// TODO: move the plugin options to Project so that they are always present.
	def compileOptions = Deprecation :: Nil
	def documentOptions: Seq[ScaladocOption] =
		LinkSource ::
		documentTitle(info.name + " " + info.currentVersion + " API") ::
		windowTitle(info.name + " " + info.currentVersion + " API") ::
		Nil
}