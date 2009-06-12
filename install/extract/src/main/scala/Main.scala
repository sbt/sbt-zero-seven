/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt.extract

import java.io.{File, InputStream}
import java.util.zip.{ZipEntry, ZipFile}

object Main
{
	lazy val log: Logger = new ConsoleLogger
	
	def main(args: Array[String])
	{
		if(args.contains("debug"))
			log.setLevel(Level.Debug)
		val result =
		Control.trapUnit("Error processing jar: ", log)
		{
			import FileUtilities.classLocationFile
			val asZip = new ZipFile(classLocationFile[Install])
			Control.trapUnitAndFinally("Error processing jar: ", log)
				{ process(asZip) }
				{ asZip.close() }
		}
		result.foreach { msg => log.error(msg); System.exit(1) }
	}
	private def process(zip: ZipFile) =
	{
		val installEntry = zip.getEntry("install")
		if(installEntry == null)
			Some("Install commands not found.")
		else
		{
			val jarAndZip = wrap.Wrappers.toList(zip.entries).filter(entry => ClasspathUtilities.isArchiveName(entry.getName)).partition(_.getName.endsWith(".jar"))
			jarAndZip match
			{
				case (Nil, _)=> Some("sbt loader not found.")
				case (_, Nil) => Some("Project to extract and build not found.")
				case (loaderEntry :: _, projectEntry :: _) => extractAndRun(zip, loaderEntry, projectEntry, installEntry)
			}
		}
	}
	private def extractAndRun(zip: ZipFile, loaderEntry: ZipEntry, projectEntry: ZipEntry, installEntry: ZipEntry) =
	{
		val zipResource = new OpenResource[ZipEntry, InputStream]
			{ protected def open(entry: ZipEntry, log: Logger) = Control.trap("Error opening " + entry.getName + " in " + zip + ": ", log) { Right(zip.getInputStream(entry)) } }
			
		def ioOption(entry: ZipEntry)(f: InputStream => Option[String]) = zipResource.ioOption(entry, "reading", log)(f)
		def io[R](entry: ZipEntry)(f: InputStream => Either[String, R]) = zipResource.io(entry, "reading", log)(f)
		
		import FileUtilities.{readString, transfer, unzip, writeStream}
		val directory = new File(".", trimExtension(projectEntry.getName))
		val loaderFile = new File(directory, loaderEntry.getName)
		assume(!directory.exists, "Could not extract project: directory " + projectEntry.getName + " exists.")

		Control.thread(io(installEntry)(readString(_, log))) { installString =>
			Control.thread(parseInstall(installString)) { install =>
				io(projectEntry)(unzip(_, Path.fromFile(directory), log)).left.toOption orElse
				writeStream(loaderFile, log) { out => ioOption(loaderEntry)(transfer(_, out, log)) } orElse
				run(loaderFile, directory, install)
			}
		}
	}
	private def parseInstall(installString: String): Either[String, Install] =
	{
		installString.split(separator) match
		{
			case Array(allOptions, allActions) =>
				val options = allOptions.split("""\n""").toList
				val actions = allActions.split("""\n""").toList
				Right( Install(options, actions) )
			case _ => Left("Invalid install script (no separator found)")
		}
	}
	private def filterEmpty(list: List[String]) = list.filter(!_.isEmpty)
	private def run(loader: File, project: File, install: Install) =
	{
		val command = "java" :: "-cp" :: loader.getAbsolutePath :: filterEmpty(install.options) ::: "sbt.boot.Boot" :: filterEmpty(install.actions)
		val builder = new java.lang.ProcessBuilder(command.toArray : _*)
		builder.directory(project)
		//import BasicIO.{processFully, transferFully}
		//val standardIO = new ProcessIO(transferFully(System.in, _, 0), processFully(System.out.println), processFully(System.err.println))
		val exitCode = ( Process(builder) ! )//( Process(builder) run standardIO).exitValue()
		if(exitCode == 0)
			None
		else
			Some("sbt exited with nonzero exit code: " + exitCode)
	}
	private def trimExtension(name: String) =
	{
		val i = name.lastIndexOf('.')
		if(i > 0) name.substring(0, i)
		else name
	}
	// keep this in sync with sbt.extract.SelfExtractingProject
	private def separator = "===================="
}
private final case class Install(options: List[String], actions: List[String]) extends NotNull