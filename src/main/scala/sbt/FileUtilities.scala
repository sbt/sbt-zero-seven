/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.{Closeable, File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.io.{ByteArrayOutputStream, InputStreamReader, OutputStreamWriter}
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter, Reader, Writer}
import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder}
import java.nio.channels.FileChannel
import java.util.jar.{JarEntry, JarOutputStream, Manifest}

object FileUtilities
{
	private val BufferSize = 8192
	private val Newline = System.getProperty("line.separator")
	
	private val PathSeparatorPattern = java.util.regex.Pattern.compile("""[;:]""")

	private[sbt] def pathSplit(s: String) = PathSeparatorPattern.split(s)
	
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

	def touch(file: File, log: Logger): Option[String] =
	{
		try
		{
			if(file.exists)
				None
			else
				createDirectory(file.getParentFile, log) orElse { file.createNewFile(); None }
		}
		catch
		{
			case e =>
			{
				log.trace(e)
				Some("Could not create file " + printableFilename(file) + ": " + e.toString)
			}
			
		}
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
			case e =>
			{
				log.trace(e)
				Some("Could not create directory " + printableFilename(dir) + ": " + e.toString)
			}
		}
	}
	def createDirectories(d: List[File], log: Logger): Option[String] =
		d match
		{
			case Nil => None
			case head :: tail => createDirectory(head, log) orElse createDirectories(tail, log)
		}
	private val MaximumTries = 10
	def createTemporaryDirectory(log: Logger) =
	{
		def create(tries: Int): Either[String, File] =
		{
			if(tries > MaximumTries)
				Left("Could not create temporary directory.")
			else
			{
				val randomName = "sbt_" + java.lang.Integer.toHexString(random.nextInt)
				val f = new File(temporaryDirectory, randomName)
				
				if(createDirectory(f, log).isEmpty)
					Right(f)
				else
					create(tries + 1)
			}
		}
		create(0)
	}

	def doInTemporaryDirectory[T](log: Logger)(action: File => Either[String, T]): Either[String, T] =
	{
		def doInDirectory(dir: File): Either[String, T] =
		{
			try
			{
				action(dir)
			}
			catch
			{
				case e => log.trace(e); Left(e.toString)
			}
			finally
			{
				delete(dir, true, log)
			}
		}
		createTemporaryDirectory(log).right.flatMap(doInDirectory)
	}
	
	def copyFlat(sources: Iterable[Path], destinationDirectory: Path, log: Logger) =
	{
		val targetSet = new scala.collection.mutable.HashSet[Path]
		copyImpl(sources, destinationDirectory, log)
		{
			source =>
			{
				val from = source.asFile
				val toPath = destinationDirectory / from.getName
				targetSet += toPath
				val to = toPath.asFile
				if(!to.exists || from.lastModified > to.lastModified && !from.isDirectory)
				{
					log.debug("Copying " + source + " to " + toPath)
					copyFile(from, to, log)
				}
				else
					None
			}
		}.toLeft(targetSet.readOnly)
	}
	private def copyImpl(sources: Iterable[Path], destinationDirectory: Path, log: Logger)
		(doCopy: Path => Option[String]): Option[String] =
	{
		val target = destinationDirectory.asFile
		val creationError =
			if(target.isDirectory)
				None
			else
				createDirectory(target, log)
		def copy(sources: List[Path]): Option[String] =
		{
			sources match
			{
				case src :: remaining =>
				{
					doCopy(src) match
					{
						case None => copy(remaining)
						case error => error
					}
				}
				case Nil => None
			}
		}
		creationError orElse
		{
			try { copy(sources.toList) }
			catch { case e => log.trace(e); Some(e.toString) }
		}
	}
	def copy(sources: Iterable[Path], destinationDirectory: Path, log: Logger) =
	{
		val targetSet = new scala.collection.mutable.HashSet[Path]
		copyImpl(sources, destinationDirectory, log)
		{
			source =>
			{
				val from = source.asFile
				val toPath = Path.fromString(destinationDirectory, source.relativePath)
				targetSet += toPath
				val to = toPath.asFile
				if(!to.exists || from.lastModified > to.lastModified)
				{
					if(from.isDirectory)
						createDirectory(to, log)
					else
					{
						log.debug("Copying " + source + " to " + toPath)
						copyFile(from, to, log)
					}
				}
				else
					None
			}
		}.toLeft(targetSet.readOnly)
	}
	
	def copyFile(sourceFile: File, targetFile: File, log: Logger): Option[String] =
	{
		require(sourceFile.exists)
		require(!sourceFile.isDirectory)
		readChannel(sourceFile, log)(
			in => writeChannel(targetFile, log) {
				out => {
					val copied = out.transferFrom(in, 0, in.size)
					if(copied == in.size)
						Right(())
					else
						Left("Could not copy '" + sourceFile + "' to '" + targetFile + "' (" + copied + "/" + in.size + " bytes copied)")
				}
			}
		).left.toOption
	}
	
	def copyDirectory(source: File, target: File, log: Logger): Option[String] =
	{
		require(source.isDirectory)
		require(!target.exists)
		def copyDirectory(sourceDir: File, targetDir: File): Option[String] =
			createDirectory(targetDir, log) orElse copyContents(sourceDir, targetDir)
		def copyContents(sourceDir: File, targetDir: File): Option[String] =
			sourceDir.listFiles.foldLeft(None: Option[String])
			{
				(result, file) =>
					result orElse
					{
						val targetFile = new File(targetDir, file.getName)
						if(file.isDirectory)
							copyDirectory(file, targetFile)
						else
							copyFile(file, targetFile, log)
					}
			}
		copyDirectory(source, target)
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
	
	private def open[T](file: File, log: Logger, constructor: File => T): Either[String, T] =
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
	private def fileOutputChannel(file: File, log: Logger) = open(file, log, (f: File) => (new FileOutputStream(f)).getChannel)
	private def fileInputChannel(file: File, log: Logger) = open(file, log, (f: File) => (new FileInputStream(f)).getChannel)
	private def fileInputStream(file: File, log: Logger) = open(file, log, (f: File) => new FileInputStream(f))
	private def fileOutputStream(file: File, log: Logger) = open(file, log, (f: File) => new FileOutputStream(f))
	private def fileWriter(charset: Charset)(file: File, log: Logger) =
		open(file, log, (f: File) => new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f), charset)) )
	private def fileReader(charset: Charset)(file: File, log: Logger) =
		open(file, log, (f: File) => new BufferedReader(new InputStreamReader(new FileInputStream(f), charset)) )
	private def io[T <: Closeable, R](file: File, open: (File, Logger) => Either[String, T],
		f: T => Either[String, R], op: String, log: Logger): Either[String, R] =
	{
		def processStream(stream: T) =
		{
			try { f(stream) }
			catch
			{
				case e: Exception => 
				{
					log.trace(e)
					Left("Error " + op + " file " + printableFilename(file) + ": " + e.getMessage)
				}
			}
			finally { closeNoException(stream) }
		}
		open(file, log).right flatMap processStream
	}
	def write(file: File, content: String, log: Logger): Option[String] = write(file, content, Charset.defaultCharset, log)
	def write(file: File, content: String, charset: Charset, log: Logger): Option[String] =
	{
		if(charset.newEncoder.canEncode(content))
			(write(file, charset, log) { w =>  w.write(content); Right(()) }).left.toOption
		else
			Some("String cannot be encoded by default charset.")
	}
	def write[R](file: File, log: Logger)(f: Writer => Either[String, R]): Either[String, R] =
		write(file, Charset.defaultCharset, log)(f)
	def write[R](file: File, charset: Charset, log: Logger)(f: Writer => Either[String, R]): Either[String, R] =
		io(file, fileWriter(charset), f, "writing", log)
	def read[R](file: File, log: Logger)(f: Reader => Either[String, R]): Either[String, R] =
		read(file, Charset.defaultCharset, log)(f)
	def read[R](file: File, charset: Charset, log: Logger)(f: Reader => Either[String, R]): Either[String, R] =
		io(file, fileReader(charset), f, "reading", log)
	// error is in Left, value is in Right
	def readString(file: File, log: Logger): Either[String, String] = readString(file, Charset.defaultCharset, log)
	def readString(file: File, charset: Charset, log: Logger): Either[String, String] =
	{
		read(file, charset, log) {
			in =>
			{
				val builder = new StringBuilder
				val buffer = new Array[Char](BufferSize)
				def readNext()
				{
					val read = in.read(buffer, 0, buffer.length)
					if(read >= 0)
					{
						builder.append(buffer, 0, read)
						readNext()
					}
					else
						None
				}
				readNext()
				Right(builder.toString)
			}
		}
	}
	def writeBytes(file: File, bytes: Array[Byte], log: Logger) =
		writeStream(file, log)	
		{ out =>
			out.write(bytes)
			Right(())
		}
	def readBytes(file: File, log: Logger): Either[String, Array[Byte]] =
		readStream(file, log)
		{ in =>
			val out = new ByteArrayOutputStream
			val buffer = new Array[Byte](BufferSize)
			def readNext()
			{
				val read = in.read(buffer)
				if(read >= 0)
				{
					out.write(buffer, 0, read)
					readNext()
				}
			}
			readNext()
			Right(out.toByteArray)
		}
		
	def writeStream[R](file: File, log: Logger)(f: OutputStream => Either[String, R]): Either[String, R] =
		io(file, fileOutputStream, f, "writing", log)
	def readStream[R](file: File, log: Logger)(f: InputStream => Either[String, R]): Either[String, R] =
		io(file, fileInputStream, f, "reading", log)
		
	def writeChannel[R](file: File, log: Logger)(f: FileChannel => Either[String, R]): Either[String, R] =
		io(file, fileOutputChannel, f, "writing", log)
	def readChannel[R](file: File, log: Logger)(f: FileChannel => Either[String, R]): Either[String, R] =
		io(file, fileInputChannel, f, "reading", log)
	
	private[sbt] def wrapNull(a: Array[File]): Array[File] =
		if(a == null)
			new Array[File](0)
		else
			a
			
	def writeLine(writer: Writer, line: String)
	{
		writer.write(line)
		writer.write(Newline)
	}
	
	val temporaryDirectory = new File(System.getProperty("java.io.tmpdir"))
	lazy val sbtJar: File = new File(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
	private val random = new java.util.Random
}