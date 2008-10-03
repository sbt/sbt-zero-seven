package sbt

abstract class CompilerCore
{
	val ClasspathOptionString = "-classpath"
	val OutputOptionString = "-d"
	
	// Returns false if there were errors, true if there were not.
	def process(args: Array[String]): Boolean
	def actionStartMessage: String
	def actionNothingToDoMessage: String
	def actionSuccessfulMessage: String
	def actionUnsuccessfulMessage: String

	def apply(sources: Iterable[Path], classpathString: String, outputDirectory: Path, options: Iterable[String], log: Logger) =
	{
		log.info(actionStartMessage)
		val classpathOption: List[String] =
			if(classpathString.isEmpty)
				Nil
			else
				List(ClasspathOptionString, classpathString)
		val outputDir = outputDirectory.asFile
		FileUtilities.createDirectory(outputDir, log) orElse
		{
			val classpathAndOut: List[String] = OutputOptionString :: outputDir.getAbsolutePath :: classpathOption
			
			try
			{
				val sourceList = sources.map(_.asFile.getAbsolutePath).toList
				if(sourceList.isEmpty)
				{
					log.info(actionNothingToDoMessage)
					None
				}
				else
				{
					val arguments = (options ++ classpathAndOut ++ sourceList).toArray
					log.debug("Arguments: " + arguments.mkString(" "))
					if(process(arguments))
					{
						log.info(actionSuccessfulMessage)
						None
					}
					else
						Some(actionUnsuccessfulMessage)
				}
			}
			catch
			{
				case e: Exception => log.trace(e); Some("Compiler error: " + e.getMessage)
			}
		}
	}
}
object Compile extends CompilerCore
{
	def process(arguments: Array[String]) =
	{
		val main = scala.tools.nsc.Main
		main.process(arguments)
		!main.reporter.hasErrors
	}
	val actionStartMessage = "Compiling..."
	val actionNothingToDoMessage = "Nothing to compile."
	val actionSuccessfulMessage = "Compilation successful."
	def actionUnsuccessfulMessage = "Compilation unsuccessful."
}
object Scaladoc extends CompilerCore
{
	def process(arguments: Array[String]) =
	{
		val main = scala.tools.nsc.ScalaDoc
		main.process(arguments)
		!main.reporter.hasErrors
	}
	val actionStartMessage = "Generating API documentation..."
	val actionNothingToDoMessage = "No sources specified."
	val actionSuccessfulMessage = "API documentation generation successful."
	def actionUnsuccessfulMessage = "API documentation generation unsuccessful."
}