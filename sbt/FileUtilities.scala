/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.{Closeable, File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.io.{FileReader, FileWriter, Reader, Writer}
import java.util.jar.{JarEntry, JarOutputStream, Manifest}

object FileUtilities
{
	val BufferSize = 8192
	val Newline = System.getProperty("line.separator")
	
	val PathSeparatorPattern = java.util.regex.Pattern.compile("""[;:]""")

	def pathSplit(s: String) = PathSeparatorPattern.split(s)
	
	def pack(sources: Iterable[Path], outputJar: Path, manifest: Manifest, recursive: Boolean, log: Logger) =
	{
		log.info("Packaging " + outputJar + " ...")
		val outputFile = outputJar.asFile
		if(outputFile.isDirectory)
			Some("Specified output file " + printableFilename(outputFile) + " is a directory.")
		else
		{
			val outputDir = outputFile.getParentFile
			val result = createDirectory(outputDir, log) orElse
				openJar(outputFile, manifest).flatMap(output => writeJar(sources, output, recursive, log))
			if(result.isEmpty)
				log.info("Packaging complete.")
			result
		}
	}
	
	private def writeJar(sources: Iterable[Path], output: JarOutputStream, recursive: Boolean, log: Logger) =
	{
		def add(source: Path)
		{
			val sourceFile = source.asFile
			if(sourceFile.isDirectory)
			{
				if(recursive)
					wrapNull(sourceFile.listFiles).foreach(file => add(source / file.getName))
			}
			else if(sourceFile.exists)
			{
				log.debug("\tAdding " + source + " ...")
				val nextEntry = new JarEntry(source.relativePath)
				nextEntry.setTime(sourceFile.lastModified)
				output.putNextEntry(nextEntry)
				transfer(new FileInputStream(sourceFile), output, log)
				output.closeEntry()
			}
			else
				log.warn("\tSource " + source + " does not exist.")
		}
		try
		{
			sources.foreach(add)
			None
		}
		catch
		{
			case e: Exception =>
				log.trace(e)
				Some("Error writing jar: " + e.getMessage)
		}
		finally { closeNoException(output) }
	}
	
	/** Creates a JarOutputStream for the given arguments.  This method properly closes
	* the underlying FileOutputStream when it is be created but the JarOutputStream
	* constructor throws an IOException.*/
	private def openJar(file: File, manifest: Manifest): Option[JarOutputStream] =
	{
		val fileStream = new FileOutputStream(file)
		var jarStream: Option[JarOutputStream] = None
		try
		{
			jarStream = Some(new JarOutputStream(fileStream, manifest))
			jarStream
		}
		catch { case e: Exception => None }
		finally
		{
			if(jarStream.isEmpty)
				closeNoException(fileStream)
		}
	}
	
	def transfer(in: InputStream, out: OutputStream, log: Logger): Option[String] =
	{
		try
		{
			val buffer = new Array[Byte](BufferSize)
			def read: None.type =
			{
				val byteCount = in.read(buffer)
				if(byteCount >= 0)
				{
					out.write(buffer, 0, byteCount)
					read
				}
				else
					None
			}
			read
		}
		catch
		{
			case e: Exception => log.trace(e); Some("Error during transfer: " + e.getMessage)
		}
		finally
		{
			closeNoException(in)
		}
	}
	def closeNoException(c: Closeable)
	{
		try { c.close }
		catch { case e: Exception => () }
	}

	def createDirectory(dir: File, log: Logger): Option[String] =
	{
		try
		{
			if(dir.exists)
			{
				if(dir.isDirectory)
					None
				else
					Some(printableFilename(dir) + " exists and is not a directory.")
			}
			else
			{
				dir.mkdirs()
				None
			}
		}
		catch
		{
			case e: Exception =>
			{
				log.trace(e)
				Some("Could not create directory " + printableFilename(dir) + ": " + e.getMessage)
			}
		}
	}
	def createDirectories(d: List[File], log: Logger): Option[String] =
		d match
		{
			case Nil => None
			case head :: tail => createDirectory(head, log) orElse createDirectories(tail, log)
		}

	def printableFilename(file: File) =
	{
		try
		{
			file.getCanonicalPath
		}
		catch
		{
			case e: Exception => file.getAbsolutePath
		}
	}
	
	def clean(files: Iterable[Path], log: Logger): Option[String] = clean(files, false, log)
	def clean(files: Iterable[Path], quiet: Boolean, log: Logger): Option[String] =
		deleteFiles(files.map(_.asFile), quiet, log)
			
	private def deleteFiles(files: Iterable[File], quiet: Boolean, log: Logger): Option[String] =
		((None: Option[String]) /: files)( (result, file) => result orElse delete(file, quiet, log))
	private def delete(file: File, quiet: Boolean, log: Logger): Option[String] =
	{
		def logMessage(message: => String)
		{
			log.log(if(quiet) Level.Debug else Level.Info, message)
		}
		try
		{
			if(file.isDirectory)
			{
				logMessage("Deleting directory " + printableFilename(file))
				deleteFiles(wrapNull(file.listFiles), true, log)
				file.delete
			}
			else if(file.exists)
			{
				logMessage("Deleting file " + printableFilename(file))
				file.delete
			}
			None
		}
		catch
		{
			case e: Exception => 
				log.trace(e)
				Some("Error deleting file " + printableFilename(file) + ": " + e.getMessage)
		}
	}
	
	def open[T](file: File, log: Logger, constructor: File => T): Either[String, T] =
	{
		val parent = file.getParentFile
		if(parent != null)
			createDirectory(parent, log)
		try { Right(constructor(file)) }
		catch
		{
			case e: Exception => 
			{
				log.trace(e)
				Left("Error opening " + printableFilename(file) + ": " + e.getMessage)
			}
		}
	}
	def fileInputStream(file: File, log: Logger) = open(file, log, (f: File) => new FileInputStream(f))
	def fileOutputStream(file: File, log: Logger) = open(file, log, (f: File) => new FileOutputStream(f))
	def fileWriter(file: File, log: Logger) = open(file, log, (f: File) => new FileWriter(f))
	def fileReader(file: File, log: Logger) = open(file, log, (f: File) => new FileReader(f))
	private def io[T <: Closeable](file: File, open: (File, Logger) => Either[String, T],
		f: T => Option[String], op: String, log: Logger): Option[String] =
	{
		open(file, log) match
		{
			case Left(errorMessage) => Some(errorMessage)
			case Right(stream) =>
				try
				{
					f(stream)
				}
				catch
				{
					case e: Exception => 
					{
						log.trace(e)
						Some("Error " + op + " file " + printableFilename(file) + ": " + e.getMessage)
					}
				}
				finally { closeNoException(stream) }
		}
	}
	def write(file: File, log: Logger)(f: Writer => Option[String]): Option[String] =
		io(file, fileWriter, f, "writing", log)
	def read(file: File, log: Logger)(f: Reader => Option[String]): Option[String] =
		io(file, fileReader, f, "reading", log)
		
	def writeStream(file: File, log: Logger)(f: OutputStream => Option[String]): Option[String] =
		io(file, fileOutputStream, f, "writing", log)
	def readStream(file: File, log: Logger)(f: InputStream => Option[String]): Option[String] =
		io(file, fileInputStream, f, "reading", log)
	
	def wrapNull(a: Array[File]): Array[File] =
		if(a == null)
			new Array[File](0)
		else
			a
			
	def writeLine(writer: Writer, line: String)
	{
		writer.write(line)
		writer.write(Newline)
	}
			
	lazy val sbtJar: File = new File(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
}