/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Steven Blundy, Mark Harrah
 */
package sbt

import scala.xml.{Elem, Group}

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
	
	def testRunner(classLoader: ClassLoader, listeners: Iterable[TestReportListener], log: Logger): TestRunner
}
trait TestRunner extends NotNull
{
	def run(testClassName: String): Result.Value
}

trait TestReportListener
{
	/** called for each class or equivalent grouping */
  def startGroup(name: String)
	/** called for each test method or equivalent */
  def testEvent(event: TestEvent)
	/** called if there was an error during test */
  def endGroup(name: String, t: Throwable)
	/** called if test completed */
  def endGroup(name: String, result: Result.Value)
}

trait TestsListener extends TestReportListener
{
	/** called once, at beginning. */
  def doInit
	/** called once, at end. */
  def doComplete(finalResult: Result.Value)
	/** called once, at end, if the test framework throws an exception. */
  def doComplete(t: Throwable)
}

abstract class WriterReportListener(val log: Logger) extends TestsListener
{
	import java.io.{IOException, PrintWriter, Writer}
	import scala.collection.mutable.{Buffer, ListBuffer}

	protected case class Summary(count: Int, failures: Int, errors: Int, skipped: Int, message: Option[String]) extends NotNull
	private var out: Option[PrintWriter] = None
	private var groupCount: Int = 0
	private var groupFailures: Int = 0
	private var groupErrors: Int = 0
	private var groupSkipped: Int = 0
	private var groupMessages: Seq[String] = Nil

	protected val passedEventHandler: TestEvent => Summary = (event: TestEvent) => event match
		{
			case SpecificationReportEvent(successes, failures, errors, skipped, desc, systems, subSpecs) => Summary(successes, failures, errors, skipped, None)
			case IgnoredEvent(name, Some(message)) => Summary(1, 0, 0, 1, Some(message.text))
			case IgnoredEvent(name, None) => Summary(1, 0, 0, 1, None)
			case _ => Summary(1, 0, 0, 0, None)
		}
	protected val failedEventHandler: TestEvent => Summary = (event: TestEvent) => event match
		{
			case FailedEvent(name, msg) => Summary(1, 1, 0, 0, Some("! " + name + ": " + msg.text))
			case TypedErrorEvent(name, event, Some(msg)) => Summary(1, 1, 0, 0, Some(event + " - " + name + ": " + msg.text))
			case TypedErrorEvent(name, event, None) => Summary(1, 1, 0, 0, Some(event + " - " + name))
			case ErrorEvent(msg) => Summary(1, 1, 0, 0, Some(msg.text))
			case SpecificationReportEvent(successes, failures, errors, skipped, desc, systems, subSpecs) => Summary(successes + failures + errors + skipped, failures, errors, skipped, Some(desc))
			case _ => {log.warn("Unrecognized failure: " + event); Summary(1, 1, 0, 0, None)}
		}
	protected val errorEventHandler: TestEvent => Summary = (event: TestEvent) => event match
		{
			case FailedEvent(name, msg) => Summary(1, 0, 1, 0, Some("! " + name + ": " + msg.text))
			case TypedErrorEvent(name, event, Some(msg)) => Summary(1, 0, 1, 0, Some(event + " - " + name + ": " + msg.text))
			case TypedErrorEvent(name, event, None) => Summary(1, 0, 1, 0, Some(event + " - " + name))
			case ErrorEvent(msg) => Summary(1, 0, 1, 0, Some(msg.text))
			case SpecificationReportEvent(successes, failures, errors, skipped, desc, systems, subSpecs) => Summary(successes + failures + errors + skipped, failures, errors, skipped, Some(desc))
			case _ => {log.warn("Unrecognized error: " + event); Summary(1, 0, 1, 0, None)}
		}
	protected def open: Writer
	protected def close =
	{
		onOut(_.close())
		out = None
	}
	def doInit = Control.trapAndLog(log){ out = Some(new PrintWriter(open)) }
	def doComplete(finalResult: Result.Value) =
	{
		finalResult match
		{
			case Result.Error => println("Error during Tests")
			case Result.Passed => println("All Tests Passed")
			case Result.Failed => println("Tests Failed")
		}
		close
	}
	def doComplete(t: Throwable) =
	{
		println("Exception in Test Framework")
		onOut(t.printStackTrace(_))
		close
	}
	def startGroup(name: String) =
	{
		groupCount = 0
		groupFailures = 0
		groupErrors = 0
		groupSkipped = 0
		groupMessages = Nil
	}
	def testEvent(event: TestEvent) = event.result match
		{
			case Some(result) =>
			{
				val Summary(count, failures, errors, skipped, msg) = result match
				{
					case Result.Passed => passedEventHandler(event)
					case Result.Failed => failedEventHandler(event)
					case Result.Error => errorEventHandler(event)
				}
				groupCount += count
				groupFailures += failures
				groupErrors += errors
				groupSkipped += skipped
				groupMessages ++= msg.toList
			}
			case None => {}
		}
	def endGroup(name: String, t: Throwable) =
	{
		groupMessages = Nil
		println("Exception in " + name)
		onOut(t.printStackTrace(_))
	}
	def endGroup(name: String, result: Result.Value) =
	{
		result match
		{
			case Result.Error => println("Error: " + name + " - Count " + groupCount + ", Failed " + groupFailures + ", Errors " + groupErrors)
			case Result.Passed => println("Passed: " + name + " - Count " + groupCount + ", Failed " + groupFailures + ", Errors " + groupErrors)
			case Result.Failed => println("Failed: " + name + " - Count " + groupCount + ", Failed " + groupFailures + ", Errors " + groupErrors)
		}
		if(!groupMessages.isEmpty)
		{
			groupMessages.foreach(println(_))
			groupMessages = Nil
			println("")
		}
	}
	protected def onOut(f: PrintWriter => Unit) = Control.trapAndLog(log){
			out match
			{
				case Some(pw) => f(pw)
				case None => log.warn("Method called when output was not open")
			}
		}
	protected def println(s: String) = onOut(_.println(s))
}

class FileReportListener(val file: Path, log: Logger) extends WriterReportListener(log)
{
	def open = new java.io.FileWriter(file.asFile)
}

abstract class TestEvent extends NotNull
{
	def result: Option[Result.Value]
}

sealed abstract class ScalaCheckEvent extends TestEvent
final case class PassedEvent(name: String, msg: Elem) extends ScalaCheckEvent { def result = Some(Result.Passed) }
final case class FailedEvent(name: String, msg: Elem) extends ScalaCheckEvent { def result = Some(Result.Failed) }

sealed abstract class ScalaTestEvent(val result: Option[Result.Value]) extends TestEvent
final case class TypedEvent(name: String, `type`: String, msg: Option[Elem])(result: Option[Result.Value]) extends ScalaTestEvent(result)
final case class TypedErrorEvent(name: String, `type`: String, msg: Option[Elem])(result: Option[Result.Value]) extends ScalaTestEvent(result)
final case class MessageEvent(msg: Elem) extends ScalaTestEvent(None)
final case class ErrorEvent(msg: Elem) extends ScalaTestEvent(None)
final case class IgnoredEvent(name: String, msg: Option[Elem]) extends ScalaTestEvent(Some(Result.Passed))

sealed abstract class SpecsEvent extends TestEvent
final case class SpecificationReportEvent(successes: Int, failures: Int, errors: Int, skipped: Int, pretty: String, systems: Seq[SystemReportEvent], subSpecs: Seq[SpecificationReportEvent]) extends SpecsEvent
{
	def result = if(errors > 0) Some(Result.Error) else if(failures > 0) Some(Result.Failed) else Some(Result.Passed)
}
final case class SystemReportEvent(description: String, verb: String, skippedSus:Option[Throwable], literateDescription: Option[Group], examples: Seq[ExampleReportEvent]) extends SpecsEvent { def result = None }
final case class ExampleReportEvent(description: String, errors: Seq[Throwable], failures: Seq[RuntimeException], skipped: Seq[RuntimeException], subExamples: Seq[ExampleReportEvent]) extends SpecsEvent { def result = None }

trait EventOutput[E <: TestEvent]
{
	def output(e: E): Unit
}

sealed abstract class LazyEventOutput[E <: TestEvent](val log: Logger) extends EventOutput[E]

final class ScalaCheckOutput(log: Logger) extends LazyEventOutput[ScalaCheckEvent](log)
{
	def output(event: ScalaCheckEvent) = event match
		{
			case PassedEvent(name, msg) => log.info("+ " + name + ": " + msg.text)
			case FailedEvent(name, msg) => log.error("! " + name + ": " + msg.text)
		}
}

final class ScalaTestOutput(log: Logger) extends LazyEventOutput[ScalaTestEvent](log)
{
	def output(event: ScalaTestEvent) = event match
		{
			case TypedEvent(name, event, Some(msg)) => log.info(event + " - " + name + ": " + msg.text)
			case TypedEvent(name, event, None) => log.info(event + " - " + name)
			case TypedErrorEvent(name, event, Some(msg)) => log.error(event + " - " + name + ": " + msg.text)
			case TypedErrorEvent(name, event, None) => log.error(event + " - " + name)
			case MessageEvent(msg) => log.info(msg.text)
			case ErrorEvent(msg) => log.error(msg.text)
			case IgnoredEvent(name, Some(msg)) => log.info("Test ignored - " + name + ": " + msg.text)
			case IgnoredEvent(name, None) => log.info("Test ignored - " + name)
		}
}

final class SpecsOutput(log: Logger) extends LazyEventOutput[SpecsEvent](log)
{
		private val Indent = "  "

	def output(event: SpecsEvent) = event match
		{
			case sre: SpecificationReportEvent => reportSpecification(sre, "")
			case sre: SystemReportEvent => reportSystem(sre, "")
			case ere: ExampleReportEvent => reportExample(ere, "")
		}

	/* The following is closely based on org.specs.runner.OutputReporter,
	* part of specs, which is Copyright 2007-2008 Eric Torreborre.
	* */

	private def reportSpecification(specification: SpecificationReportEvent, padding: String)
	{
		val newIndent = padding + Indent
		reportSpecifications(specification.subSpecs, newIndent)
		reportSystems(specification.systems, newIndent)
	}
	private def reportSpecifications(specifications: Iterable[SpecificationReportEvent], padding: String)
	{
		for(specification <- specifications)
			reportSpecification(specification, padding)
	}
	private def reportSystems(systems: Iterable[SystemReportEvent], padding: String)
	{
		for(system <- systems)
			reportSystem(system, padding)
	}
	private def reportSystem(sus: SystemReportEvent, padding: String)
	{
		log.info(padding + sus.description + " " + sus.verb + sus.skippedSus.map(" (skipped: " + _.getMessage + ")").getOrElse(""))
		for(description <- sus.literateDescription)
			log.info(padding + description.text)
		reportExamples(sus.examples, padding)
		log.info(" ")
	}
	private def reportExamples(examples: Iterable[ExampleReportEvent], padding: String)
	{
		for(example <- examples)
		{
			reportExample(example, padding)
			reportExamples(example.subExamples, padding + Indent)
		}
	}
	private def status(example: ExampleReportEvent) =
	{
		if (example.errors.size + example.failures.size > 0)
			"x "
		else if (example.skipped.size > 0)
			"o "
		else
			"+ "
	}
	private def reportExample(example: ExampleReportEvent, padding: String)
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
}
abstract class BasicTestRunner extends TestRunner
{
	protected def log: Logger
	protected def listeners: Seq[TestReportListener]

	final def run(testClass: String): Result.Value =
	{
		safeListenersCall(_.startGroup(testClass))
		try
		{
			val result = runTest(testClass)
			safeListenersCall(_.endGroup(testClass, result))
			result
		}
		catch
		{
			case e =>
			{
				safeListenersCall(_.endGroup(testClass, e))
				Result.Error
			}
		}
	}
	def runTest(testClass: String): Result.Value

	protected def fire(event: TestEvent) = safeListenersCall(_.testEvent(event))
	protected def safeListenersCall(call: (TestReportListener) => Unit) = listeners.foreach(l => Control.trapAndLog(log){call(l)})
}

object TestFramework
{
	import scala.collection.{Map, Set}
	def runTests(frameworks: Iterable[TestFramework], classpath: Iterable[Path], tests: Iterable[TestDefinition], log: Logger, listeners: Iterable[TestReportListener]): Option[String] =
	{
		val mappedTests = testMap(frameworks, tests)
		if(mappedTests.isEmpty)
		{
			log.info("No tests to run.")
			None
		}
		else
			Control.trapUnit("", log) { doRunTests(classpath, mappedTests, log, listeners) }
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
	private def doRunTests(classpath: Iterable[Path], tests: Map[TestFramework, Set[String]], log: Logger, listeners: Iterable[TestReportListener]): Option[String] =
	{
		val loader: ClassLoader = new IntermediateLoader(classpath.map(_.asURL).toSeq.toArray, getClass.getClassLoader)
		val testsListeners = listeners.filter(_.isInstanceOf[TestsListener]).map(_.asInstanceOf[TestsListener])
		import Result._
		var result: Value = Passed
		testsListeners.foreach(l => Control.trapAndLog(log){ l.doInit })
		try {
			for((framework, testClassNames) <- tests)
			{
				if(testClassNames.isEmpty)
					log.debug("No tests to run for framework " + framework.name + ".")
				else
				{
					log.info(" ")
					log.info("Running " + framework.name + " tests...")
					val runner = framework.testRunner(loader, listeners, log)
					for(testClassName <- testClassNames)
					{
						runner.run(testClassName) match
						{
							case Error => result = Error
							case Failed => if(result != Error) result = Failed
							case _ => ()
						}
					}
				}
			}
			testsListeners.foreach(l => Control.trapAndLog(log){ l.doComplete(result) })
		}
		catch
		{
			case t =>
			{
				testsListeners.foreach(l => Control.trapAndLog(log){ l.doComplete(t) })
				throw t
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
	final def testRunner(projectLoader: ClassLoader, listeners: Iterable[TestReportListener], log: Logger): TestRunner =
	{
		val runnerClassName = testRunnerClassName
		val lazyLoader = new LazyFrameworkLoader(runnerClassName, Array(FileUtilities.sbtJar.toURI.toURL), projectLoader, getClass.getClassLoader)
		val runnerClass = Class.forName(runnerClassName, true, lazyLoader).asSubclass(classOf[TestRunner])

		runnerClass.getConstructor(classOf[Logger], classOf[Seq[TestReportListener]], classOf[ClassLoader]).newInstance(log, Seq(new LogTestReportListener(log, lazyLoader)) ++ listeners, projectLoader)
	}

	private class LogTestReportListener(val log: Logger, val testLoader: ClassLoader) extends TestReportListener
	{
		import LazyTestFramework._
		private lazy val scalaCheckOutput: EventOutput[ScalaCheckEvent] = load(ScalaCheckOutputClass)
		private lazy val scalaTestOutput: EventOutput[ScalaTestEvent] = load(ScalaTestOutputClass)
		private lazy val specsOutput: EventOutput[SpecsEvent] = load(SpecsOutputClass)

		def startGroup(name: String)
		{
			log.info("")
			log.info("Testing " + name + " ...")
		}
		def testEvent(event: TestEvent) =
		{
			log.debug("in testEvent:" + event)
			event match
			{
				case sce: ScalaCheckEvent => scalaCheckOutput.output(sce)
				case ste: ScalaTestEvent => scalaTestOutput.output(ste)
				case se: SpecsEvent => specsOutput.output(se)
				case e => log.error("Event not supported: " + e)
			}
		}
		def endGroup(name: String, t: Throwable)
		{
			log.error("Could not run test " + name + ": " + t.toString)
			log.trace(t)
		}
		def endGroup(name: String, result: Result.Value) = { log.debug("in endGroup:" + result) }

		private def load[E <: TestEvent](className: String): LazyEventOutput[E] =
		{
			val lazyLoader = new LazyFrameworkLoader(className, Array(FileUtilities.sbtJar.toURI.toURL), testLoader, getClass.getClassLoader)
			val formatterClass = Class.forName(className, true, lazyLoader).asSubclass(classOf[LazyEventOutput[E]])
			formatterClass.getConstructor(classOf[Logger]).newInstance(log)
		}
	}
}

object LazyTestFramework
{
	private val ScalaCheckOutputClass = "sbt.ScalaCheckOutput"
	private val ScalaTestOutputClass = "sbt.ScalaTestOutput"
	private val SpecsOutputClass = "sbt.SpecsOutput"
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
private class ScalaCheckRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	import org.scalacheck.{Pretty, Properties, Test}
	def runTest(testClassName: String): Result.Value =
	{
		val test = ModuleUtilities.getObject(testClassName, testLoader).asInstanceOf[Properties]
		if(Test.checkProperties(test, Test.defaultParams, propReport, testReport).find(!_._2.passed).isEmpty)
			Result.Passed
		else
			Result.Failed
	}
	private def propReport(pName: String, s: Int, d: Int) {}
	private def testReport(pName: String, res: Test.Result) =
	{
		if(res.passed)
			fire(PassedEvent(pName, <message>{Pretty.pretty(res)}</message>))
		else
			fire(FailedEvent(pName, <message>{Pretty.pretty(res)}</message>))
	}
}
/** The test runner for ScalaTest suites. */
private class ScalaTestRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	def runTest(testClassName: String): Result.Value =
	{
		import org.scalatest.{Stopper, Suite}
		val testClass = Class.forName(testClassName, true, testLoader).asSubclass(classOf[Suite])
		val test = testClass.newInstance
		val reporter = new ScalaTestReporter
		val stopper = new Stopper { override def stopRequested = false }
		test.execute(None, reporter, stopper, Set.empty, Set.empty, Map.empty, None)
		if(reporter.succeeded)
			Result.Passed
		else
			Result.Failed
	}
	
	/** An implementation of Reporter for ScalaTest. */
	private class ScalaTestReporter extends org.scalatest.Reporter with NotNull
	{
		import org.scalatest.Report
		override def testIgnored(report: Report) =
		{
			if(report.message.trim.isEmpty) fire(IgnoredEvent(report.name, None))
			else fire(IgnoredEvent(report.name, Some(<message>report.message.trim</message>)))
		}
		override def testStarting(report: Report) { info(report, "Test starting", None) }
		override def testSucceeded(report: Report) { info(report, "Test succeeded", Some(Result.Passed)) }
		override def testFailed(report: Report)
		{
			succeeded = false
			error(report, "Test failed", Some(Result.Failed))
		}
		
		override def infoProvided(report : Report) { info(report, "", None) }
		
		override def suiteStarting(report: Report) { info(report, "Suite starting", None) }
		override def suiteCompleted(report: Report) { info(report, "Suite completed", None) }
		override def suiteAborted(report: Report) { error(report, "Suite aborted", None) }
		
		override def runStarting(testCount: Int) { fire(MessageEvent(<message>Run starting</message>)) }
		override def runStopped()
		{
			succeeded = false
			fire(ErrorEvent(<message>Run stopped</message>))
		}
		override def runAborted(report: Report)
		{
			succeeded = false
			error(report, "Run aborted", None)
		}
		override def runCompleted() { log.info("Run completed.") }
		
		private def error(report: Report, event: String, result: Option[Result.Value]) { logReport(report, event, result, Level.Error) }
		private def info(report: Report, event: String, result: Option[Result.Value]) { logReport(report, event, result, Level.Info) }
		private def logReport(report: Report, event: String, result: Option[Result.Value], level: Level.Value)
		{
			level match
			{
				case Level.Error =>
					if(report.message.trim.isEmpty)
						fire(TypedErrorEvent(report.name, event, None)(result))
					else
						fire(TypedErrorEvent(report.name, event, Some(<message>{report.message.trim}</message>))(result))
				case Level.Info =>
					if(report.message.trim.isEmpty)
						fire(TypedEvent(report.name, event, None)(result))
					else
						fire(TypedEvent(report.name, event, Some(<message>{report.message.trim}</message>))(result))
				case l => log.warn("Level not prepared for:" + l)
			}
		}
		
		var succeeded = true
	}
}
/** The test runner for specs tests. */
private class SpecsRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	import org.specs.Specification
	import org.specs.runner.TextFormatter
	import org.specs.specification.{Example, Sus}

	def runTest(testClassName: String): Result.Value =
	{
		val test = ModuleUtilities.getObject(testClassName, testLoader).asInstanceOf[Specification]
		val event = reportSpecification(test)
		fire(event)
		if(test.isFailing)
			Result.Failed
		else
			Result.Passed
	}
	
	/* The following is closely based on org.specs.runner.OutputReporter,
	* part of specs, which is Copyright 2007-2008 Eric Torreborre.
	* */
	
	private def reportSpecification(spec: Specification): SpecificationReportEvent =
	{
		return SpecificationReportEvent(spec.successes.size, spec.failures.size, spec.errors.size, spec.skipped.size, spec.pretty, reportSystems(spec.systems), reportSpecifications(spec.subSpecifications))
	}
	private def reportSpecifications(specifications: Seq[Specification]): Seq[SpecificationReportEvent] =
	{
		for(specification <- specifications) yield
			reportSpecification(specification)
	}
	private def reportSystems(systems: Seq[Sus]): Seq[SystemReportEvent] =
	{
		for(system <- systems) yield
			reportSystem(system)
	}
	private def reportSystem(sus: Sus): SystemReportEvent =
	{
		def format(description: Option[Elem]): Option[Group] = description match
			{
				case None => None
				case Some(elem) => Some(new TextFormatter().format(elem, sus.examples))
			}

		SystemReportEvent(sus.description, sus.verb, sus.skippedSus, format(sus.literateDescription), reportExamples(sus.examples))
	}
	private def reportExamples(examples: Seq[Example]): Seq[ExampleReportEvent] =
	{
		for(example <- examples) yield
			reportExample(example)
	}
	private def reportExample(example: Example): ExampleReportEvent =
	{
		ExampleReportEvent(example.description, example.errors, example.failures, example.skipped, reportExamples(example.subExamples))
	}
}
