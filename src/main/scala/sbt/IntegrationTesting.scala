/* sbt -- Simple Build Tool
 * Copyright 2009  Steven Blundy
 */
package sbt

trait IntegrationTesting
{
	/** Override to provide pre-suite setup. */
	protected def pretests: Option[String] = None
	/** Override to provide post-suite cleanup. */
	protected def posttests: Option[String] = None
}

trait ScalaIntegrationTesting extends IntegrationTesting
{
	self: ScalaProject =>

	protected def integrationTestTask(frameworks: Iterable[TestFramework], classpath: PathFinder, analysis: CompileAnalysis, options: => Seq[TestOption]) =
		task{
			import Control._
			trapUnit("Setup failed: ", log)(pretests) match
			{
				case Some(msg) => Some(msg)
				case None => trapUnitAndFinally("Exception in framework", log)(executeIntegrationTests(frameworks, classpath, analysis, options))(
					trapUnit("Cleanup failed: ", log)(posttests) match
					{
						case Some(msg) => log.error(msg)
						case None => {}
					}
				)
			}
		}

	private def executeIntegrationTests(frameworks: Iterable[TestFramework], classpath: PathFinder, analysis: CompileAnalysis, options: => Seq[TestOption]): Option[String] =
		doTests(frameworks, classpath, analysis, options)
}

/** A fully featured integration testing that may be mixed in with any subclass of <code>BasicScalaProject</code>.
 * Pre-suite setup and post-suite cleanup are provide by overriding <code>pretests</code> and <code>posttests</code> respectively.*/
trait BasicScalaIntegrationTesting extends ScalaIntegrationTesting with IntegrationTestPaths
{
	self: BasicScalaProject =>

	import BasicScalaIntegrationTesting._
	
	lazy val integrationTestCompile = integrationTestCompileAction
	lazy val integrationTest = integrationTestAction

	val integrationTestCompileConditional = new CompileConditional(integrationTestCompileConfiguration)

	protected def integrationTestAction = integrationTestTask(integrationTestFrameworks, integrationTestClasspath, integrationTestCompileConditional.analysis, integrationTestOptions) dependsOn integrationTestCompile describedAs IntegrationTestCompileDescription
	protected def integrationTestCompileAction = integrationTestCompileTask() describedAs IntegrationTestDescription

	protected def integrationTestCompileTask() = task{ integrationTestCompileConditional.run }

	def integrationTestOptions: Seq[TestOption] = Nil
	def integrationTestCompileOptions = testCompileOptions
	def integrationTestClasspath = integrationTestCompilePath +++ fullClasspath(Configurations.Test, false)
	def integrationTestSources = descendents(integrationTestScalaSourcePath, "*.scala")
	def integrationTestLabel = "integration-test"
	def integrationTestCompileConfiguration = new IntegrationTestCompileConfig

	def integrationTestFrameworks = testFrameworks

	class IntegrationTestCompileConfig extends BaseCompileConfig
	{
		def label = integrationTestLabel
		def sources = integrationTestSources
		def outputDirectory = integrationTestCompilePath
		def classpath = integrationTestClasspath
		def analysisPath = integrationTestAnalysisPath
		def options = integrationTestCompileOptions.map(_.asString)
		def testDefinitionClassNames = integrationTestFrameworks.map(_.testSuperClassName)
	}
}

object BasicScalaIntegrationTesting
{
	val IntegrationTestCompileDescription = "Compiles integration test sources."
	val IntegrationTestDescription = "Runs all integration tests detected during compilation."
}

trait IntegrationTestPaths extends BasicProjectPaths
{
	import IntegrationTestPaths._

	def integrationTestDirectoryName = DefaultIntegrationTestDirectoryName
	def integrationTestCompileDirectoryName = DefaultIntegrationTestCompileDirectoryName
	def integrationTestAnalysisDirectoryName = DefaultIntegrationTestAnalysisDirectoryName

	def integrationTestSourcePath = sourcePath / integrationTestDirectoryName
	def integrationTestScalaSourcePath = integrationTestSourcePath / scalaDirectoryName

	def integrationTestCompilePath = outputPath / integrationTestCompileDirectoryName
	def integrationTestAnalysisPath = outputPath / integrationTestAnalysisDirectoryName
}

object IntegrationTestPaths
{
	val DefaultIntegrationTestDirectoryName = "it"
	val DefaultIntegrationTestCompileDirectoryName = "it-classes"
	val DefaultIntegrationTestAnalysisDirectoryName = "it-analysis"
}