/** This tests implementing a test framework in a project definition.  To ensure sbt's builtin ScalaCheck
* test framework is not used, it flips success and failure so that a failing test is marked as succeeding and
* a suceeding test is marked as failing. */

package framework

import sbt._

/** This is required until after 0.4.5, at which point the LazyTestFramework in sbt will be unsealed*/
abstract class LazyFramework extends TestFramework
{
	/** The class name of the the test runner that executes
	* tests for this framework.*/
	protected def testRunnerClassName: String
	
	/** Creates an instance of the runner given by 'testRunnerClassName'.*/
	final def testRunner(projectLoader: ClassLoader, listeners: Iterable[TestReportListener], log: Logger): TestRunner =
	{
		val runnerClassName = testRunnerClassName
		val lazyLoader = new LazyLoader(runnerClassName, Array(FileUtilities.sbtJar.toURI.toURL), projectLoader, getClass.getClassLoader)
		val runnerClass = Class.forName(runnerClassName, true, lazyLoader).asSubclass(classOf[TestRunner])

		runnerClass.getConstructor(classOf[Logger], classOf[Seq[TestReportListener]], classOf[ClassLoader]).newInstance(log, listeners, projectLoader)
	}
}

object FrameworkScalaCheck extends LazyFramework
{
	val name = "ScalaCheck"

	def testSuperClassName = "org.scalacheck.Properties"
	def testSubClassType = ClassType.Module

	def testRunnerClassName = "framework.RunnerScalaCheck"
}

class RunnerScalaCheck(val log: Logger, val listeners: Seq[TestReportListener], val testLoader: ClassLoader) extends BasicTestRunner
{
	import org.scalacheck.{Pretty, Properties, Test}
	def runTest(testClassName: String): Result.Value =
	{
		val test = 
		try {
			ModuleUtilities.getObject(testClassName, testLoader).asInstanceOf[Properties]
		} catch {
			case e => e.printStackTrace; throw e
		}
		val result = Test.checkProperties(test, Test.defaultParams, propReport, testReport).find(!_._2.passed)
		if(result.isEmpty)
			Result.Failed // intentionally flipped (see top comment)
		else
			Result.Passed // intentionally flipped (see top comment)
	}
	private def propReport(pName: String, s: Int, d: Int) {}
	private def testReport(name: String, res: Test.Result)
	{
		val msg = Pretty.pretty(res)
		if(res.passed)
			log.info("+ " + name + ": " + msg)
		else
			log.error("! " + name + ": " + msg)
		
	}
}



import java.io.File
import java.net.{URI, URL, URLClassLoader}


private abstract class BaseLoader(urls: Array[URL], parent: ClassLoader) extends URLClassLoader(urls, parent) with NotNull
{
	require(parent != null) // included because a null parent is legitimate in Java
	@throws(classOf[ClassNotFoundException])
	override final def loadClass(className: String, resolve: Boolean): Class[_] =
	{
		val loaded = findLoadedClass(className)
		val found =
			if(loaded == null)
				doLoadClass(className)
			else
				loaded
			
		if(resolve)
			resolveClass(found)
		found
	}
	protected def doLoadClass(className: String): Class[_]
	protected final def selfLoadClass(className: String): Class[_] = super.loadClass(className, false)
}
private class LazyLoader(runnerClassName: String, urls: Array[URL], parent: ClassLoader, grandparent: ClassLoader)
	extends BaseLoader(urls, parent) with NotNull
{
	def doLoadClass(className: String): Class[_] =
	{
		if(Loaders.isNestedOrSelf(className, runnerClassName))
		{
			if(Loaders.isSbtClass(className))
				findClass(className)
			else
				grandparent.loadClass(className)
		}
		else if(Loaders.isSbtClass(className)) // we circumvent the parent loader because we know that we want the
			grandparent.loadClass(className)              // version of sbt that is currently the builder (not the project being built)
		else
			parent.loadClass(className)
	}
}
private object Loaders
{
	val SbtPackage = "sbt."
	def isNestedOrSelf(className: String, checkAgainst: String) =
		className == checkAgainst || className.startsWith(checkAgainst + "$")
	def isSbtClass(className: String) = className.startsWith(Loaders.SbtPackage)
}