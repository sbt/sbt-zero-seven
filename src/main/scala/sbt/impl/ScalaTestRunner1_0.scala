/* sbt -- Simple Build Tool
 * Copyright 2009  Josh Cough
 */
package sbt.impl

import org.scalatest._

/**The test runner for ScalaTest suites. It is compiled in a second step after the rest of sbt.*/
class ScalaTestRunner1_0(val log: Logger, val listeners: Seq[TestReportListener],
                      val testLoader: ClassLoader) extends BasicTestRunner
{
  import java.lang.reflect.Modifier

  def runTest(testClassName: String): Result.Value = {
    val testClass = Class.forName(testClassName, true, testLoader).asSubclass(classOf[Suite])

    if( isAccessibleSuite(testClass)){
      val test = testClass.newInstance
      val reporter = new ScalaTestReporter
      test.run(None, reporter, new Stopper {}, Filter(), Map.empty, None, new Tracker)
      if (reporter.succeeded) Result.Passed else Result.Failed
    }
   else
      Result.Passed
  }

  private val emptyClassArray = new Array[java.lang.Class[T] forSome { type T }](0)

  private def isAccessibleSuite(clazz: java.lang.Class[_]): Boolean = {

    try {
      classOf[Suite].isAssignableFrom(clazz) &&
      Modifier.isPublic(clazz.getModifiers) &&
      !Modifier.isAbstract(clazz.getModifiers) &&
      Modifier.isPublic(clazz.getConstructor(emptyClassArray: _*).getModifiers)

    } catch {
      case nsme: NoSuchMethodException => false
      case se: SecurityException => false
    }
  }

  /**An implementation of Reporter for ScalaTest. */
  private class ScalaTestReporter extends Reporter with NotNull
  {
    import org.scalatest.events._
    var succeeded = true

    def apply(event: Event) {
      event match {
      // why log.info sometimes and fire(MessageEvent...) other times?

        case rc: RunCompleted => log.info("Run completed.")
        case rs: RunStarting => fire(MessageEvent("Run starting"))
        case rs: RunStopped => {succeeded = false; fire(ErrorEvent("Run stopped"))}

        case ra: RunAborted => {succeeded = false; fire(ErrorEvent("Run aborted"))}

        // this one seems to be working really well
        case ts: TestStarting => fire(TypedEvent(ts.testName, "Test Starting", None)(None))

        // not sure what to do here at all
        case tp: TestPending =>
        case tf: TestFailed => {succeeded = false; fire(TypedErrorEvent(tf.testName, "Test Failed", None, tf.throwable)(Some(Result.Failed)))}

        // this one also seems to be working really well
        case ts: TestSucceeded => fire(TypedEvent(ts.testName, "Test Succeeded", None)(Some(Result.Passed)))
        // need to check if there is a reason why this test was ignored

        case ti: TestIgnored => fire(IgnoredEvent(ti.testName, Some("test ignored")))

        case sc: SuiteCompleted => fire(TypedEvent(sc.suiteName, "Suite Completed", None)(None))
        // why not sure Some(Result.Failed) here?

        // also, why not say succeeded = false?
        // seems like i should do both if the suite is aborted.
        case sa: SuiteAborted => fire(TypedErrorEvent(sa.suiteName, "Suite Aborted", Some(sa.message), sa.throwable)(None))

        case ss: SuiteStarting => fire(TypedEvent(ss.suiteName, "Suite Starting", None)(None))

        // not actually sure if this is what i should do here...info provided is really just...some random extra info provided by a test, like a log statement
        case ip: InfoProvided => fire(MessageEvent(ip.message))
      }
    }
  }
}