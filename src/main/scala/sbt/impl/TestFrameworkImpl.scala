/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Steven Blundy, Mark Harrah
 */

package sbt.impl

import scala.xml.{Elem, Group}

/* The following classes run tests for their associated test framework.
* NOTE #1: DO NOT actively use these classes.  Only specify their names to LazyTestFramework
*  for reflective loading.  This allows using the test libraries provided on the
*  project classpath instead of requiring global versions.
* NOTE #2: Keep all active uses of these test frameworks inside these classes so that sbt
*  runs without error when a framework is not available at runtime and no tests for that
*  framework are defined.*/

/** The test runner for ScalaCheck tests. */
private[sbt] class ScalaCheckRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
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
private[sbt] class ScalaTestRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
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
private[sbt] class SpecsRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
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
