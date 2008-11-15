/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
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
	
	def testRunner(classLoader: ClassLoader, log: Logger): TestRunner
}
trait TestRunner extends NotNull
{
	def run(testClassName: String): Result.Value
}
abstract class BasicTestRunner extends TestRunner
{
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
			case e: Exception =>
			{
				log.error("Could not run test " + testClass + ": " + e.toString)
				log.trace(e)
				Result.Error
			}
		}
	}
	def runTest(testClass: String): Result.Value
}

import scala.collection.{Map, Set}
object TestFramework
{
	def runTests(frameworks: Iterable[TestFramework], classpath: Iterable[Path], tests: Iterable[TestDefinition], log: Logger): Option[String] =
	{
		val mappedTests = testMap(frameworks, tests)
		if(mappedTests.isEmpty)
		{
			log.info("No tests to run.")
			None
		}
		else
			doRunTests(classpath, mappedTests, log)
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
	private def doRunTests(classpath: Iterable[Path], tests: Map[TestFramework, Set[String]], log: Logger): Option[String] =
	{
		val loader = getLoader(classpath)
		
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
				val runner = framework.testRunner(loader, log)
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
		
		result match
		{
			case Error => Some("ERROR occurred during testing.")
			case Failed => Some("One or more tests FAILED.")
			case Passed =>
			{
				log.info("All tests PASSED.")
				None
			}
		}
	}
	private def getLoader(classpath: Iterable[Path]): ClassLoader =
	{
		import java.net.URLClassLoader
		new URLClassLoader(classpath.map(_.asURL).toSeq.toArray, getClass.getClassLoader)
	}
}
import java.net.{URL, URLClassLoader}
private class LazyFrameworkLoader(runnerClassName: String, urls: Array[URL], parent: ClassLoader)
	extends URLClassLoader(urls, parent)
{
	@throws(classOf[ClassNotFoundException])
	override def loadClass(className: String, resolve: Boolean): Class[_] =
	{
		val loaded = findLoadedClass(className)
		val found =
			if(loaded == null)
			{
				if(className.startsWith(runnerClassName)) // this is required to get nested classes and closures
					findClass(className)
				else
					super.loadClass(className, resolve)
			}
			else
				loaded
			
		if(resolve)
			resolveClass(found)
		found
	}
}
sealed abstract class LazyTestFramework extends TestFramework
{
	/** The class name of the the test runner that executes
	* tests for this framework.*/
	protected def testRunnerClassName: String
	
	/** Creates an instance of the runner given by 'testRunnerClassName'.*/
	final def testRunner(loader: ClassLoader, log: Logger): TestRunner =
	{
		val runnerClassName = testRunnerClassName
		val lazyLoader = new LazyFrameworkLoader(runnerClassName, Array(FileUtilities.sbtJar.toURI.toURL), loader)
		val runnerClass = Class.forName(runnerClassName, true, lazyLoader).asSubclass(classOf[TestRunner])
		runnerClass.getConstructor(classOf[Logger]).newInstance(log)
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
private class ScalaCheckRunner(val log: Logger) extends BasicTestRunner
{
	def runTest(testClass: String): Result.Value =
	{
		import org.scalacheck.{Test, Properties}
		val test = ModuleUtilities.getObject(testClass, getClass.getClassLoader).asInstanceOf[Properties]
		if(Test.checkProperties(test).find(!_._2.passed).isEmpty)
			Result.Passed
		else
			Result.Failed
	}
}
/** The test runner for ScalaTest suites. */
private class ScalaTestRunner(val log: Logger) extends BasicTestRunner
{
	def runTest(testClassName: String): Result.Value =
	{
		import org.scalatest.Suite
		val testClass = Class.forName(testClassName, true, getClass.getClassLoader).asSubclass(classOf[Suite])
		val test = testClass.newInstance
		test.execute() // TODO: implement Reporter to get result!
		Result.Passed
	}
}
/** The test runner for specs tests. */
private class SpecsRunner(val log: Logger) extends BasicTestRunner
{
	def runTest(testClass: String): Result.Value =
	{
		import org.specs.Specification
		//import org.specs.runner.ConsoleRunner
		val test = ModuleUtilities.getObject(testClass, getClass.getClassLoader).asInstanceOf[Specification]
		//val runner = new ConsoleRunner(test)
		//runner.report(test :: Nil)
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