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
	protected def safeListenersCall(call: (TestReportListener) => Unit) = TestFramework.safeForeach(listeners, log)(call)
}

object TestFramework
{
	private val ScalaCompilerJarPackages = "scala.tools.nsc." :: "jline." :: "ch.epfl.lamp." :: Nil

	private[sbt] def safeForeach[T](it: Iterable[T], log: Logger)(f: T => Unit): Unit = it.foreach(i => Control.trapAndLog(log){ f(i) } )
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
		val filterCompilerLoader = new FilteredLoader(getClass.getClassLoader, ScalaCompilerJarPackages)
		val loader: ClassLoader = new IntermediateLoader(classpath.map(_.asURL).toSeq.toArray, filterCompilerLoader)
		val oldLoader = Thread.currentThread.getContextClassLoader
		Thread.currentThread.setContextClassLoader(loader)
		val testsListeners = listeners.filter(_.isInstanceOf[TestsListener]).map(_.asInstanceOf[TestsListener])
		def foreachListenerSafe(f: TestsListener => Unit): Unit = safeForeach(testsListeners, log)(f)
		
		import Result.{Error,Passed,Failed}
		var result: Result.Value = Passed
		foreachListenerSafe(_.doInit)
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
			foreachListenerSafe(_.doComplete(result))
		}
		catch
		{
			case t =>
			{
				foreachListenerSafe(_.doComplete(t))
				throw t
			}
		}
		finally {
			Thread.currentThread.setContextClassLoader(oldLoader)
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

abstract class LazyTestFramework extends TestFramework
{
	/** The class name of the the test runner that executes
	* tests for this framework.*/
	protected def testRunnerClassName: String
	
	/** Creates an instance of the runner given by 'testRunnerClassName'.*/
	final def testRunner(projectLoader: ClassLoader, listeners: Iterable[TestReportListener], log: Logger): TestRunner =
	{
		val runnerClassName = testRunnerClassName
		val frameworkClasspath = FileUtilities.classLocation(getClass)
		val sbtURL = FileUtilities.sbtJar.toURI.toURL
		val lazyLoader = new LazyFrameworkLoader(runnerClassName, Array(frameworkClasspath, sbtURL), projectLoader, getClass.getClassLoader)
		val runnerClass = Class.forName(runnerClassName, true, lazyLoader).asSubclass(classOf[TestRunner])

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
	def testSubClassType = ClassType.Module
	
	def testRunnerClassName = "sbt.impl.SpecsRunner"
}