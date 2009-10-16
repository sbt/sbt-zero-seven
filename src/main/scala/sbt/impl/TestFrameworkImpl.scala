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
			fire(PassedEvent(pName, pretty(res)))
		else
			fire(FailedEvent(pName, pretty(res)))
	}
	private def pretty(res: Test.Result): String =
	{
		try { pretty1_5(res) }
		catch { case e: NoSuchMethodError => pretty1_6(res) }
	}
	private def pretty1_5(res: Test.Result): String = Pretty.pretty(res)
	private def pretty1_6(res: Test.Result): String =
	{
		// the following is equivalent to: (Pretty.prettyTestRes(res))(Pretty.defaultParams)
		// and is necessary because of binary incompatibility in Pretty between ScalaCheck 1.5 and 1.6
		val loader = getClass.getClassLoader
		val prettyObj = ModuleUtilities.getObject("org.scalacheck.Pretty", loader)
		val prettyInst = prettyObj.getClass.getMethod("prettyTestRes", classOf[Test.Result]).invoke(prettyObj, res)
		val defaultParams = prettyObj.getClass.getMethod("defaultParams").invoke(prettyObj)
		prettyInst.getClass.getMethod("apply", Class.forName("org.scalacheck.Pretty$Params", true, loader)).invoke(prettyInst, defaultParams).toString
	}
}
/** The test runner for ScalaTest suites. */
private[sbt] class ScalaTestRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	private[this] lazy val runner0_9 = get0_9Runner
	private[this] lazy val runner1_0 = get1_0Runner
	def runTest(testClassName: String): Result.Value =
	{
		try { runner0_9.runTest(testClassName) }
		catch {
			case e: NoClassDefFoundError if e.getMessage.contains("Reporter$class") =>
				runner1_0.runTest(testClassName)
		}
	}

	private[this] def get0_9Runner = new ScalaTestRunner0_9(log, listeners, testLoader)
	private[this] def get1_0Runner =
	{
		val clz = Class.forName("sbt.impl.ScalaTestRunner1_0", true, getClass.getClassLoader)
		val inst = clz.getConstructor(classOf[Logger], classOf[Seq[TestReportListener]], classOf[ClassLoader]).newInstance(log, listeners, testLoader)
		inst.asInstanceOf[BasicTestRunner]
	}
}
private[sbt] class ScalaTestRunner0_9(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	def runTest(testClassName: String): Result.Value =
	{
		import org.scalatest.{Stopper, Suite}
		val testClass = Class.forName(testClassName, true, testLoader).asSubclass(classOf[Suite])
		val test = testClass.newInstance
		val reporter = new ScalaTestReporter
		val stopper = new Stopper { override def stopRequested = false }
		test.execute(None, reporter, stopper, Set.empty, Set("org.scalatest.Ignore"), Map.empty, None)
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
			else fire(IgnoredEvent(report.name, Some(report.message.trim)))
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

		override def runStarting(testCount: Int) { fire(MessageEvent("Run starting")) }
		override def runStopped()
		{
			succeeded = false
			fire(ErrorEvent("Run stopped"))
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
						fire(TypedErrorEvent(report.name, event, None, report.throwable)(result))
					else
						fire(TypedErrorEvent(report.name, event, Some(report.message.trim), report.throwable)(result))
				case Level.Info =>
					if(report.message.trim.isEmpty)
						fire(TypedEvent(report.name, event, None)(result))
					else
						fire(TypedEvent(report.name, event, Some(report.message.trim))(result))
				case l => log.warn("Level not expected:" + l)
			}
		}

		var succeeded = true
	}
}
/** The test runner for specs tests. */
private[sbt] class SpecsRunner(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	import org.specs.Specification
	import org.specs.specification.{Example, Sus}

	def runTest(testClassName: String) = error("Use the two argument variant")
	override def runTest(testClassName: String, isModule: Boolean): Result.Value =
	{
		val test =
			if(isModule)
				ModuleUtilities.getObject(testClassName, testLoader).asInstanceOf[Specification]
			else
				Class.forName(testClassName, true, testLoader).asSubclass(classOf[Specification]).newInstance
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
		 // this is for binary compatibility between specs 1.4.x and 1.5.0: the ancestor of Specification containing these two methods changed
		val reflectedSpec: { def systems: Seq[Sus]; def subSpecifications: Seq[Specification] } = spec

		return SpecificationReportEvent(spec.successes.size, spec.failures.size, spec.errors.size, spec.skipped.size, spec.pretty,
			reportSystems(reflectedSpec.systems), reportSpecifications(reflectedSpec.subSpecifications))
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
			// for source compatibility between specs 1.4.x and 1.5.0:
			// in specs 1.5.0, description is LiterateDescription
			// in specs < 1.5.0, description is Elem
			// LiterateDescription.desc is a Node
			// Elem.child is a Seq[Node]
			// each has a map[T](f: Node => T): Seq[T] defined so we use reflection to call the right method

			//description.child.map(_.text) // Elem equivalent
			//description.desc.map(_.text)  // LiterateDescription
		def formatDescription(a: AnyRef) =
		{
			val toMap = 
				try { call(a, "desc") }
				catch { case e: Exception => call(a, "child") }
			val mapText = (a: AnyRef) => a.getClass.getMethod("text").invoke(a)
			toMap.getClass.getMethod("map", Class.forName("scala.Function1")).invoke(toMap, mapText).asInstanceOf[Seq[String]]
		}
		def call(a: AnyRef, m: String) = a.getClass.getMethod(m).invoke(a)
		def format: Option[Seq[String]] =
		{
			val litD = sus.literateDescription
			if(litD.isEmpty) None else Some(formatDescription(litD.get))
		}
		// these are for 1.6 compatibility, which removed skippedSus (skipped still exists) and examples (moved to specification)
		def skipped(sus: Sus) = classOf[Sus].getMethod("skipped").invoke(sus).asInstanceOf[Seq[Throwable]]
		def examples(sus: Sus) =
		{
			try { sus.examples } // we compile against specs 1.4.x, which has examples directly on Sus  so this compiles
			catch { case _: NoSuchMethodError =>  // It fails at runtime for specs 1.6 because examples is now on BaseSpecification
				val spec = classOf[Sus].getMethod("specification").invoke(sus)
				spec.getClass.getMethod("examples").invoke(spec).asInstanceOf[List[Example]]
			}
		}
		SystemReportEvent(sus.description, sus.verb, skipped(sus), format, reportExamples(examples(sus)))
	}
	private def reportExamples(examples: Seq[Example]): Seq[ExampleReportEvent] =
	{
		for(example <- examples) yield
			reportExample(example)
	}
	private def reportExample(example: Example): ExampleReportEvent =
	{
		def examples(example: Example) =
			try { example.subExamples } // we compile against specs 1.4.x, which has subExamples defined on Example, so this compiles
			catch { case _ : NoSuchMethodError =>  // It fails at runtime for specs 1.6 because examples is the new method
				classOf[Example].getMethod("examples").invoke(example).asInstanceOf[Seq[Example]]
			}
		ExampleReportEvent(example.description, example.errors, example.failures, example.skipped, reportExamples(examples(example)))
	}
}
