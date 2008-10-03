package sbt

object ScalaCheckTests
{
	val TestClassName = "org.scalacheck.Test"
	val PropertiesClassName = "org.scalacheck.Properties"
	object Result extends Enumeration
	{
		val Error, Passed, Failed = Value
	}
	def apply(classpath: Iterable[Path], tests: Iterable[String], log: Logger): Option[String] =
	{
		import java.net.URLClassLoader
		val loader = new URLClassLoader(classpath.map(_.asFile.toURI.toURL).toSeq.toArray, getClass.getClassLoader)
		val testSingleton = ModuleUtilities.getObject(TestClassName, loader)
		val propertiesClass = Class.forName(PropertiesClassName, true, loader)
		val checkMethod = testSingleton.getClass.getMethod("checkProperties", propertiesClass)
		
		def runTest(testClass: String): Result.Value =
		{
			log.info("")
			log.info("Testing " + testClass + " ...")
			try
			{
				val test = ModuleUtilities.getObject(testClass, loader)
				type ResultType = { def passed: Boolean }
				val resultsAny = checkMethod.invoke(testSingleton, test)
				val results = resultsAny.asInstanceOf[Seq[(String, ResultType)]]
				if(results.find(!_._2.passed).isEmpty)
					Result.Passed
				else
					Result.Failed
			}
			catch
			{
				case e: Exception =>
				{
					log.error("Could not run test " + testClass + ": " + e.getMessage)
					log.trace(e)
					Result.Error
				}
			}
		}
		
		import Result._
		var result: Value = Passed
		for(testClass <- tests)
		{
			runTest(testClass) match
			{
				case Error => result = Error
				case Failed => if(result != Error) result = Failed
				case _ => ()
			}
		}
		result match
		{
			case Error => Some("ERROR occurred during testing.")
			case Failed => Some("One or more tests FAILED.")
			case Passed =>
			{
				log.info("All tests PASSED")
				None
			}
		}
	}
}