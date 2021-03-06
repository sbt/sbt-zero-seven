/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Steven Blundy, Mark Harrah
 */
package sbt

object Result extends Enumeration
{
	val Error, Passed, Failed = Value
}
object ClassType extends Enumeration
{
	val Module, Class, ClassOrModule = Value
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
	def run(testClassName: String, isModule: Boolean): Result.Value
}

abstract class BasicTestRunner extends TestRunner
{
	protected def log: Logger
	protected def listeners: Seq[TestReportListener]

	final def run(testClass: String, isModule: Boolean): Result.Value =
	{
		safeListenersCall(_.startGroup(testClass))
		try
		{
			val result = runTest(testClass, isModule)
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
	def runTest(testClass: String, isModule: Boolean): Result.Value = runTest(testClass)
	def runTest(testClass: String): Result.Value

	protected def fire(event: TestEvent) = safeListenersCall(_.testEvent(event))
	protected def safeListenersCall(call: (TestReportListener) => Unit) = TestFramework.safeForeach(listeners, log)(call)
}

final class NamedTestTask(val name: String, action: => Option[String]) extends NotNull { def run() = action }
object TestFramework
{
	def runTests(frameworks: Iterable[TestFramework], classpath: Iterable[Path], tests: Iterable[TestDefinition], log: Logger,
		listeners: Iterable[TestReportListener]) =
	{
		val (start, runTests, end) = testTasks(frameworks, classpath, tests, log, listeners, true, Nil, Nil)
		def run(tasks: Iterable[NamedTestTask]) = tasks.foreach(_.run())
		run(start)
		run(runTests)
		run(end)
	}

	private val ScalaCompilerJarPackages = "scala.tools.nsc." :: "jline." :: "ch.epfl.lamp." :: Nil

	private val TestStartName = "test-start"
	private val TestFinishName = "test-finish"

	private[sbt] def safeForeach[T](it: Iterable[T], log: Logger)(f: T => Unit): Unit = it.foreach(i => Control.trapAndLog(log){ f(i) } )
	import scala.collection.{Map, Set}
	def testTasks(frameworks: Iterable[TestFramework], classpath: Iterable[Path], tests: Iterable[TestDefinition], log: Logger,
		listeners: Iterable[TestReportListener], endErrorsEnabled: Boolean, setup: Iterable[() => Option[String]],
		cleanup: Iterable[() => Option[String]]): (Iterable[NamedTestTask], Iterable[NamedTestTask], Iterable[NamedTestTask]) =
	{
		val mappedTests = testMap(frameworks, tests)
		if(mappedTests.isEmpty)
			(new NamedTestTask(TestStartName, None) :: Nil, Nil, new NamedTestTask(TestFinishName, { log.info("No tests to run."); None }) :: Nil )
		else
			createTestTasks(classpath, mappedTests, log, listeners, endErrorsEnabled, setup, cleanup)
	}
	private def testMap(frameworks: Iterable[TestFramework], tests: Iterable[TestDefinition]): Map[TestFramework, Set[TestDefinition]] =
	{
		import scala.collection.mutable.{HashMap, HashSet, Set}
		val map = new HashMap[TestFramework, Set[TestDefinition]]
		if(!frameworks.isEmpty)
		{
			for(test <- tests)
			{
				def isTestForFramework(framework: TestFramework) =
					classTypeMatches(framework.testSubClassType, test.isModule) &&
					framework.testSuperClassName == test.superClassName

				for(framework <- frameworks.find(isTestForFramework))
					map.getOrElseUpdate(framework, new HashSet[TestDefinition]) += test
			}
		}
		wrap.Wrappers.readOnly(map)
	}
	private def classTypeMatches(classType: ClassType.Value, isModule: Boolean) =
		classType match
		{
			case ClassType.ClassOrModule => true
			case ClassType.Module => isModule
			case ClassType.Class => !isModule
		}
	private def createTasks(work: Iterable[() => Option[String]], baseName: String) =
		work.toList.zipWithIndex.map{ case (work, index) => new NamedTestTask(baseName + " " + (index+1), work()) }

	private def createTestTasks(classpath: Iterable[Path], tests: Map[TestFramework, Set[TestDefinition]], log: Logger,
		listeners: Iterable[TestReportListener], endErrorsEnabled: Boolean, setup: Iterable[() => Option[String]],
		cleanup: Iterable[() => Option[String]]) =
	{
		val filterCompilerLoader = new FilteredLoader(getClass.getClassLoader, ScalaCompilerJarPackages)
		val loader: ClassLoader = new IntermediateLoader(classpath.map(_.asURL).toSeq.toArray, filterCompilerLoader)
		val testsListeners = listeners.filter(_.isInstanceOf[TestsListener]).map(_.asInstanceOf[TestsListener])
		def foreachListenerSafe(f: TestsListener => Unit): Unit = safeForeach(testsListeners, log)(f)

		import Result.{Error,Passed,Failed}
		object result
		{
			private[this] var value: Result.Value = Passed
			def apply() = synchronized { value }
			def update(v: Result.Value): Unit = synchronized { if(value != Error) value = v }
		}
		val startTask = new NamedTestTask(TestStartName, {foreachListenerSafe(_.doInit); None}) :: createTasks(setup, "Test setup")
		val testTasks =
			tests flatMap { case (framework, tests) =>

					val runner = framework.testRunner(loader, listeners, log)
					for(test <- tests) yield
					{
						def runTest() =
						{
							val oldLoader = Thread.currentThread.getContextClassLoader
							Thread.currentThread.setContextClassLoader(loader)
							try {
								runner.run(test.testClassName, test.isModule) match
								{
									case Error => result() = Error; Some("ERROR occurred during testing.")
									case Failed => result() = Failed; Some("Test FAILED")
									case _ => None
								}
							}
							finally {
								Thread.currentThread.setContextClassLoader(oldLoader)
							}
						}
						new NamedTestTask(test.testClassName, runTest())
					}
			}
		def end() =
		{
			foreachListenerSafe(_.doComplete(result()))
			result() match
			{
				case Error => if(endErrorsEnabled) Some("ERROR occurred during testing.") else None
				case Failed => if(endErrorsEnabled) Some("One or more tests FAILED.") else None
				case Passed =>
				{
					log.info(" ")
					log.info("All tests PASSED.")
					None
				}
			}
		}
		val endTask = new NamedTestTask(TestFinishName, end() ) :: createTasks(cleanup, "Test cleanup")
		(startTask, testTasks, endTask)
	}
}

abstract class LazyTestFramework extends TestFramework
{
	/** The class name of the the test runner that executes
	* tests for this framework.*/
	protected def testRunnerClassName: String

	protected def bundledTestRunnerClassName: Option[String] = None
	protected def extraNames: Seq[String] = Nil

	/** Creates an instance of the runner given by 'testRunnerClassName'.*/
	final def testRunner(projectLoader: ClassLoader, listeners: Iterable[TestReportListener], log: Logger): TestRunner =
	{
		val frameworkClasspath = FileUtilities.classLocation(getClass)
		val sbtURL = FileUtilities.sbtJar.toURI.toURL
		val projectClasspath = projectLoader.asInstanceOf[java.net.URLClassLoader].getURLs
		val loaderURLs = Array(frameworkClasspath, sbtURL) ++ projectClasspath

		def load(name: String): Option[Class[_]] =
		{
			val selfNames: Seq[String] = extraNames ++ Seq(name)
			val lazyLoader = new LazyFrameworkLoader(selfNames, loaderURLs, projectLoader, getClass.getClassLoader)
			try { Some(Class.forName(name, true, lazyLoader)) }
			catch { case e: ClassNotFoundException => None }
		}
		val c = bundledTestRunnerClassName.flatMap(load).orElse(load(testRunnerClassName))
		val runnerClass = c.getOrElse(error("Could not find test runner")).asSubclass(classOf[TestRunner])
		runnerClass.getConstructor(classOf[Logger], classOf[Seq[TestReportListener]], classOf[ClassLoader]).newInstance(log, listeners, projectLoader)
	}
}

/** The test framework definition for ScalaTest.*/
object ScalaTestFramework extends LazyTestFramework
{
	val name = "ScalaTest"
	val SuiteClassName = "org.scalatest.Suite"

	def testSuperClassName = SuiteClassName
	def testSubClassType = ClassType.Class

	def testRunnerClassName = "sbt.impl.ScalaTestRunner"
	override def bundledTestRunnerClassName = Some("org.scalatest.sbt.ScalaTestRunner")
	override def extraNames = "sbt.impl.ScalaTestRunner1_0" :: "sbt.impl.ScalaTestRunner0_9" :: Nil
}
/** The test framework definition for ScalaCheck.*/
object ScalaCheckFramework extends LazyTestFramework
{
	val name = "ScalaCheck"
	val PropertiesClassName = "org.scalacheck.Properties"

	def testSuperClassName = PropertiesClassName
	def testSubClassType = ClassType.Module

	def testRunnerClassName = "sbt.impl.ScalaCheckRunner"
}
/** The test framework definition for specs.*/
object SpecsFramework extends LazyTestFramework
{
	val name = "specs"
	val SpecificationClassName = "org.specs.Specification"

	def testSuperClassName = SpecificationClassName
	def testSubClassType = ClassType.ClassOrModule

	def testRunnerClassName = "sbt.impl.SpecsRunner"
}