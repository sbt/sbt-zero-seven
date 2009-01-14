/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import org.scalacheck.{Pretty, Properties, Test}
import org.scalatest.Report
import org.specs.Specification
import org.specs.specification.{Example, Sus}
import org.specs.runner.TextFormatter

object Result extends Enumeration
{
	val Error, Passed, Failed = Value
}
object ClassType extends Enumeration
{
	val Module, Class = Value
}

trait TestFramework extends NotNull
{
	def name: String
	def testSuperClassName: String
	def testSubClassType: ClassType.Value

	def testRunner(classLoader: ClassLoader, log: Logger, reporters: Seq[TestReporter]): TestRunner
}
trait TestRunner extends NotNull
{
	def run(testClassName: String): Result.Value
}

trait TestReporter extends NotNull
{
	def doInit: Unit
	def doComplete: Unit
}

trait ScalaCheckTestReporter extends TestReporter
{
	def propReport(pName: String, s: Int, d: Int)
	def testReport(pName: String, res: Test.Result)
}
trait ScalaTestTestReporter extends TestReporter
{
	val scalaTestReporter: org.scalatest.Reporter
}
trait SpecsTestReporter extends TestReporter
{
	def reportSpecification(specification: Specification, padding: String)
}

class LogTestReporter(val log: Logger) extends ScalaCheckTestReporter with ScalaTestTestReporter with SpecsTestReporter {
	val Indent = "  "
	def propReport(pName: String, s: Int, d: Int) {}
	def testReport(pName: String, res: Test.Result) =
	{
		import Pretty._
		val s = (if(res.passed) "+ " else "! ") + pName + ": " + pretty(res)
		if(res.passed)
			log.info(s)
		else
			log.error(s)
	}


//	def error(report: Report, event: String) { logReport(report, event, Level.Error) }
//	def info(report: Report, event: String) { logReport(report, event, Level.Info) }
	val scalaTestReporter = new org.scalatest.Reporter()
	{
		override def testIgnored(report: Report) { info(report, "Test ignored") }
		override def testStarting(report: Report) { info(report, "Test starting") }
		override def testSucceeded(report: Report) { info(report, "Test succeeded") }
		override def testFailed(report: Report) { error(report, "Test failed") }

		override def infoProvided(report : Report) { info(report, "") }

		override def suiteStarting(report: Report) { info(report, "Suite starting") }
		override def suiteCompleted(report: Report) { info(report, "Suite completed") }
		override def suiteAborted(report: Report) { error(report, "Suite aborted") }

		override def runStarting(testCount: Int) { log.info("Run starting") }
		override def runStopped() { log.error("Run stopped.") }
		override def runAborted(report: Report) { error(report, "Run aborted") }
		override def runCompleted() { log.info("Run completed.") }

		private def error(report: Report, event: String) { logReport(report, event, Level.Error) }
		private def info(report: Report, event: String) { logReport(report, event, Level.Info) }
		private def logReport(report: Report, event: String, level: Level.Value)
		{
			val trimmed = report.message.trim
			val message = if(trimmed.isEmpty) "" else ": " + trimmed
			log.log(level, event + " - " + report.name + message)
		}
	}

	private def logReport(report: Report, event: String, level: Level.Value)
	{
		val trimmed = report.message.trim
		val message = if(trimmed.isEmpty) "" else ": " + trimmed
		log.log(level, event + " - " + report.name + message)
	}

	/* The following is closely based on org.specs.runner.OutputReporter,
	* part of specs, which is Copyright 2007-2008 Eric Torreborre.
	* */

	def reportSpecification(specification: Specification, padding: String)
	{
		val newIndent = padding + Indent
		reportSpecifications(specification.subSpecifications, newIndent)
		reportSystems(specification.systems, newIndent)
	}
	private def reportSpecifications(specifications: Iterable[Specification], padding: String)
	{
		for(specification <- specifications)
			reportSpecification(specification, padding)
	}
	private def reportSystems(systems: Iterable[Sus], padding: String)
	{
		for(system <- systems)
			reportSystem(system, padding)
	}
	private def reportSystem(sus: Sus, padding: String)
	{
		log.info(padding + sus.description + " " + sus.verb + sus.skippedSus.map(" (skipped: " + _.getMessage + ")").getOrElse(""))
		for(description <- sus.literateDescription)
			log.info(padding + new TextFormatter().format(description, sus.examples).text)
		reportExamples(sus.examples, padding)
		log.info(" ")
	}
	private def reportExamples(examples: Iterable[Example], padding: String)
	{
		for(example <- examples)
		{
			reportExample(example, padding)
			reportExamples(example.subExamples, padding + Indent)
		}
	}
	private def status(example: Example) =
	{
		if (example.errors.size + example.failures.size > 0)
			"x "
		else if (example.skipped.size > 0)
			"o "
		else
			"+ "
	}
	private def reportExample(example: Example, padding: String)
	{
		log.info(padding + status(example) + example.description)
		for(skip <- example.skipped)
		{
			log.trace(skip)
			log.warn(padding + skip.toString)
		}
		for(e <- example.failures ++ example.errors)
		{
			log.trace(e)
			log.error(padding + e.toString)
		}
	}

	def doInit = {}
	def doComplete = {}
}

abstract class BasicTestRunner[TR <: TestReporter](testReporters: Seq[TestReporter]) extends TestRunner
{
	protected val reporters = testReporters.map(tr => tr.asInstanceOf[TR])
	protected def log: Logger
	final def run(testClass: String): Result.Value =
	{
		log.info("")
		log.info("Testing " + testClass + " ...")
		try
		{
			runTest(testClass)
		}
		catch
		{
			case e =>
			{
				log.error("Could not run test " + testClass + ": " + e.toString)
				log.trace(e)
				Result.Error
			}
		}
	}
	def runTest(testClass: String): Result.Value
}

object TestFramework
{
	import scala.collection.{Map, Set}
	def runTests(frameworks: Iterable[TestFramework], classpath: Iterable[Path], tests: Iterable[TestDefinition], log: Logger, reporters: Seq[TestReporter]): Option[String] =
	{
		val mappedTests = testMap(frameworks, tests)
		val reporters = Seq(new LogTestReporter(log))
		if(mappedTests.isEmpty)
		{
			log.info("No tests to run.")
			None
		}
		else
			Control.trapUnit("", log) { doRunTests(classpath, mappedTests, log, reporters) }
	}
	private def testMap(frameworks: Iterable[TestFramework], tests: Iterable[TestDefinition]): Map[TestFramework, Set[String]] =
	{
		import scala.collection.mutable.{HashMap, HashSet, Set}
		val map = new HashMap[TestFramework, Set[String]]
		if(!frameworks.isEmpty)
		{
			for(test <- tests)
			{
				def isTestForFramework(framework: TestFramework) =
					(framework.testSubClassType == ClassType.Module) == test.isModule &&
					framework.testSuperClassName == test.superClassName

				for(framework <- frameworks.find(isTestForFramework))
					map.getOrElseUpdate(framework, new HashSet[String]) += test.testClassName
			}
		}
		map.readOnly
	}
	private def doRunTests(classpath: Iterable[Path], tests: Map[TestFramework, Set[String]], log: Logger, reporters: Seq[TestReporter]): Option[String] =
	{
		val loader: ClassLoader = new IntermediateLoader(classpath.map(_.asURL).toSeq.toArray, getClass.getClassLoader)

		import Result._
		var result: Value = Passed
		for((framework, testClassNames) <- tests)
		{
			if(testClassNames.isEmpty)
				log.debug("No tests to run for framework " + framework.name + ".")
			else
			{
				log.info(" ")
				log.info("Running " + framework.name + " tests...")
				val runner = framework.testRunner(loader, log, reporters)
				reporters.foreach(r => r.doInit)
				for(testClassName <- testClassNames)
				{
					runner.run(testClassName) match
					{
						case Error => result = Error
						case Failed => if(result != Error) result = Failed
						case _ => ()
					}
				}
				reporters.foreach(r => r.doComplete)
			}
		}

		result match
		{
			case Error => Some("ERROR occurred during testing.")
			case Failed => Some("One or more tests FAILED.")
			case Passed =>
			{
				log.info(" ")
				log.info("All tests PASSED.")
				None
			}
		}
	}
}
sealed abstract class LazyTestFramework extends TestFramework
{
	/** The class name of the the test runner that executes
	* tests for this framework.*/
	protected def testRunnerClassName: String

	/** Creates an instance of the runner given by 'testRunnerClassName'.*/
	final def testRunner(projectLoader: ClassLoader, log: Logger, reporters: Seq[TestReporter]): TestRunner =
	{
		val runnerClassName = testRunnerClassName
		val lazyLoader = new LazyFrameworkLoader(runnerClassName, Array(FileUtilities.sbtJar.toURI.toURL), projectLoader, getClass.getClassLoader)
		val runnerClass = Class.forName(runnerClassName, true, lazyLoader).asSubclass(classOf[TestRunner])
		runnerClass.getConstructor(classOf[Logger], classOf[Seq[TestReporter]], classOf[ClassLoader]).newInstance(log, reporters, projectLoader)
	}
}
/** The test framework definition for ScalaTest.*/
object ScalaTestFramework extends LazyTestFramework
{
	val name = "ScalaTest"
	val SuiteClassName = "org.scalatest.Suite"

	def testSuperClassName = SuiteClassName
	def testSubClassType = ClassType.Class

	def testRunnerClassName = "sbt.ScalaTestRunner"
}
/** The test framework definition for ScalaCheck.*/
object ScalaCheckFramework extends LazyTestFramework
{
	val name = "ScalaCheck"
	val PropertiesClassName = "org.scalacheck.Properties"

	def testSuperClassName = PropertiesClassName
	def testSubClassType = ClassType.Module

	def testRunnerClassName = "sbt.ScalaCheckRunner"
}
/** The test framework definition for specs.*/
object SpecsFramework extends LazyTestFramework
{
	val name = "specs"
	val SpecificationClassName = "org.specs.Specification"

	def testSuperClassName = SpecificationClassName
	def testSubClassType = ClassType.Module

	def testRunnerClassName = "sbt.SpecsRunner"
}
/* The following classes run tests for their associated test framework.
* NOTE #1: DO NOT actively use these classes.  Only specify their names to LazyTestFramework
*  for reflective loading.  This allows using the test libraries provided on the
*  project classpath instead of requiring global versions.
* NOTE #2: Keep all active uses of these test frameworks inside these classes so that sbt
*  runs without error when a framework is not available at runtime and no tests for that
*  framework are defined.*/

/** The test runner for ScalaCheck tests. */
private class ScalaCheckRunner(val log: Logger, testReporters: Seq[TestReporter], testLoader: ClassLoader) extends BasicTestRunner[ScalaCheckTestReporter](testReporters)
{
	def runTest(testClassName: String): Result.Value =
	{
		val test = ModuleUtilities.getObject(testClassName, testLoader).asInstanceOf[Properties]
		if(Test.checkProperties(test, Test.defaultParams, propReport, testReport).find(!_._2.passed).isEmpty)
			Result.Passed
		else
			Result.Failed
	}
	private def propReport(pName: String, s: Int, d: Int) = reporters.foreach(r => r.propReport(pName, s, d))
	private def testReport(pName: String, res: Test.Result) = reporters.foreach(r => r.testReport(pName, res))
}
/** The test runner for ScalaTest suites. */
private class ScalaTestRunner(val log: Logger, testReporters: Seq[TestReporter], testLoader: ClassLoader) extends BasicTestRunner[ScalaTestTestReporter](testReporters)
{
	def runTest(testClassName: String): Result.Value =
	{
		import org.scalatest.{Stopper, Suite}
		val testClass = Class.forName(testClassName, true, testLoader).asSubclass(classOf[Suite])
		val test = testClass.newInstance
		val reporter = new ScalaTestReporter(reporters.map(r => r.scalaTestReporter))
		val stopper = new Stopper { override def stopRequested = false }
		test.execute(None, reporter, stopper, Set.empty, Set.empty, Map.empty, None)
		if(reporter.succeeded)
			Result.Passed
		else
			Result.Failed
	}

	/** An implementation of Reporter for ScalaTest.*/
	private class ScalaTestReporter(reporters: Seq[org.scalatest.Reporter]) extends org.scalatest.Reporter with NotNull
	{
		override def testIgnored(report: Report) = reporters.foreach(r => r.testIgnored(report))
		override def testStarting(report: Report) = reporters.foreach(r => r.testStarting(report))
		override def testSucceeded(report: Report) = reporters.foreach(r => r.testSucceeded(report))
		override def testFailed(report: Report)
		{
			succeeded = false
			reporters.foreach(r => r.testFailed(report))
		}

		override def infoProvided(report : Report) = reporters.foreach(r => r.infoProvided(report))

		override def suiteStarting(report: Report) = reporters.foreach(r => r.suiteStarting(report))
		override def suiteCompleted(report: Report) = reporters.foreach(r => r.suiteCompleted(report))
		override def suiteAborted(report: Report) = reporters.foreach(r => r.suiteAborted(report))

		override def runStarting(testCount: Int) = reporters.foreach(r => r.runStarting(testCount))
		override def runStopped()
		{
			succeeded = false
			reporters.foreach(r => r.runStopped())
		}
		override def runAborted(report: Report)
		{
			succeeded = false
			reporters.foreach(r => r.runAborted(report))
		}
		override def runCompleted() = reporters.foreach(r => r.runCompleted())

		var succeeded = true
	}
}
/** The test runner for specs tests. */
private class SpecsRunner(val log: Logger, testReporters: Seq[TestReporter], testLoader: ClassLoader) extends BasicTestRunner[SpecsTestReporter](testReporters)
{
	val Indent = "  "
	def runTest(testClassName: String): Result.Value =
	{
		val test = ModuleUtilities.getObject(testClassName, testLoader).asInstanceOf[Specification]
		reporters.foreach(r => r.reportSpecification(test, ""))
		if(test.isFailing)
			Result.Failed
		else
			Result.Passed
	}
}


/* The following implements the simple syntax for storing test definitions.
* The syntax is:
*
* definition := isModule? className separator className
* isModule := '<module>'
* separator := '<<'
*/

import scala.util.parsing.combinator._

import TestParser._
/** Represents a test implemented by 'testClassName' of type 'superClassName'.*/
case class TestDefinition(isModule: Boolean, testClassName: String, superClassName: String) extends NotNull
{
	override def toString =
		(if(isModule) IsModuleLiteral else "") + testClassName + SubSuperSeparator + superClassName
}
class TestParser extends RegexParsers with NotNull
{
	def test: Parser[TestDefinition] =
		( isModule ~! className ~! SubSuperSeparator ~! className ) ^^
			{ case module ~ testName ~ SubSuperSeparator ~ superName => TestDefinition(module, testName.trim, superName.trim) }
	def isModule: Parser[Boolean] = (IsModuleLiteral?) ^^ (_.isDefined)
	def className: Parser[String] = ClassNameRegexString.r
	
	def parse(testDefinitionString: String): Either[String, TestDefinition] =
	{
		def parseError(msg: String) = Left("Could not parse test definition '" + testDefinitionString + "': " + msg)
		parseAll(test, testDefinitionString) match
		{
			case Success(result, next) => Right(result)
			case err: NoSuccess => parseError(err.msg)
		}
	}
}
object TestParser
{
	val IsModuleLiteral = "<module>"
	val SubSuperSeparator = "<<"
	val ClassNameRegexString = """[^<]+"""
	def parse(testDefinitionString: String): Either[String, TestDefinition] = (new TestParser).parse(testDefinitionString)
}