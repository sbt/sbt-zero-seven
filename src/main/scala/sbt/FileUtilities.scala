/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import java.io.{Closeable, File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.io.{ByteArrayOutputStream, InputStreamReader, OutputStreamWriter}
import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter, Reader, Writer}
import java.net.URL
import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder}
import java.nio.channels.FileChannel
import java.util.jar.{JarEntry, JarOutputStream, Manifest}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

/** A collection of file related methods. */
object FileUtilities
{
	/** The size of the byte or char buffer used in various methods.*/
	private val BufferSize = 8192
	private val Newline = System.getProperty("line.separator")
	/** A pattern used to split a String by path separator characters.*/
	private val PathSeparatorPattern = java.util.regex.Pattern.compile("""[;:]""")

	/** Splits a String around path separator characters. */
	private[sbt] def pathSplit(s: String) = PathSeparatorPattern.split(s)
	
	def jar(sources: Iterable[Path], outputJar: Path, manifest: Manifest, recursive: Boolean, log: Logger) =
		archive(sources, outputJar, Some(manifest), recursive, log)
	@deprecated def pack(sources: Iterable[Path], outputJar: Path, manifest: Manifest, recursive: Boolean, log: Logger) =
		jar(sources, outputJar, manifest, recursive, log)
	def zip(sources: Iterable[Path], outputZip: Path, recursive: Boolean, log: Logger) =
		archive(sources, outputZip, None, recursive, log)
	
	private def archive(sources: Iterable[Path], outputPath: Path, manifest: Option[Manifest], recursive: Boolean, log: Logger) =
	{
		log.info("Packaging " + outputPath + " ...")
		val outputFile = outputPath.asFile
		if(outputFile.isDirectory)
			Some("Specified output file " + printableFilename(outputFile) + " is a directory.")
		else
		{
			val outputDir = outputFile.getParentFile
			val result = createDirectory(outputDir, log) orElse
				withZipOutput(outputFile, manifest, log)
				{ output =>
					val createEntry: (String => ZipEntry) = if(manifest.isDefined) new JarEntry(_) else new ZipEntry(_)
					writeZip(sources, output, recursive, log)(createEntry)
				}
			if(result.isEmpty)
				log.info("Packaging complete.")
			result
		}
	}
	
	private def writeZip(sources: Iterable[Path], output: ZipOutputStream, recursive: Boolean, log: Logger)(createEntry: String => ZipEntry) =
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
				val nextEntry = createEntry(source.relativePath)
				nextEntry.setTime(sourceFile.lastModified)
				output.putNextEntry(nextEntry)
				transferAndClose(new FileInputStream(sourceFile), output, log)
				output.closeEntry()
			}
			else
				log.warn("\tSource " + source + " does not exist.")
		}
		sources.foreach(add)
		None
	}
	
	private def withZipOutput(file: File, manifest: Option[Manifest], log: Logger)(f: ZipOutputStream => Option[String]): Option[String] =
	{
		writeStream(file: File, log: Logger)
		{
			fileOut =>
			{
				val (zipOut, ext) =
					manifest match
					{
						case Some(mf) => (new JarOutputStream(fileOut, mf), "jar")
						case None => (new ZipOutputStream(fileOut), "zip")
					}
				Control.trapUnitAndFinally("Error writing " + ext + ": ", log)
					{ f(zipOut) } { zipOut.close }
			}
		}
	}
	import scala.collection.Set
	def unzip(from: Path, toDirectory: Path, log: Logger): Either[String, Set[Path]] =
		unzip(from, toDirectory, AllPassFilter, log)
	def unzip(from: File, toDirectory: Path, log: Logger): Either[String, Set[Path]] =
		unzip(from, toDirectory, AllPassFilter, log)
	def unzip(from: InputStream, toDirectory: Path, log: Logger): Either[String, Set[Path]] =
		unzip(from, toDirectory, AllPassFilter, log)
	def unzip(from: URL, toDirectory: Path, log: Logger): Either[String, Set[Path]] =
		unzip(from, toDirectory, AllPassFilter, log)
	
	def unzip(from: Path, toDirectory: Path, filter: NameFilter, log: Logger): Either[String, Set[Path]] =
		unzip(from.asFile, toDirectory, filter, log)
	def unzip(from: File, toDirectory: Path, filter: NameFilter, log: Logger): Either[String, Set[Path]] =
		readStreamValue(from, log)(in => unzip(in, toDirectory, filter, log))
	def unzip(from: URL, toDirectory: Path, filter: NameFilter, log: Logger): Either[String, Set[Path]] =
		readStreamValue(from, log) { stream => unzip(stream, toDirectory, filter, log) }
	def unzip(from: InputStream, toDirectory: Path, filter: NameFilter, log: Logger): Either[String, Set[Path]] =
	{
		createDirectory(toDirectory, log) match
		{
			case Some(err) => Left(err)
			case None =>
			{
				val zipInput = new ZipInputStream(from)
				Control.trapAndFinally("Error unzipping: ", log)
				{ extract(zipInput, toDirectory, filter, log) }
				{ zipInput.close() }
			}
		}
	}
	private def extract(from: ZipInputStream, toDirectory: Path, filter: NameFilter, log: Logger) =
	{
		val set = new scala.collection.mutable.HashSet[Path]
		def next(): Option[String] =
		{
			val entry = from.getNextEntry
			if(entry == null)
				None
			else
			{
				val name = entry.getName
				val result =
					if(filter.accept(name))
					{
						val target = Path.fromString(toDirectory, name)
						log.debug("Extracting zip entry '" + name + "' to '" + target + "'")
						val result =
							if(entry.isDirectory)
								createDirectory(target, log)
							else
								writeStream(target.asFile, log) { out => FileUtilities.transfer(from, out, log) }
						target.asFile.setLastModified(entry.getTime)
						result
					}
					else
					{
						log.debug("Ignoring zip entry '" + name + "'")
						None
					}
				from.closeEntry()
				result match { case None => next(); case x => x }
			}
		}
		next().toLeft(set.readOnly)
	}
	
	def transfer(in: InputStream, out: OutputStream, log: Logger): Option[String] =
		transferImpl(in, out, false, log)
	def transferAndClose(in: InputStream, out: OutputStream, log: Logger): Option[String] =
		transferImpl(in, out, true, log)
	private def transferImpl(in: InputStream, out: OutputStream, close: Boolean, log: Logger): Option[String] =
	{
		Control.trapUnitAndFinally("Error during transfer: ", log)
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
		{ if(close) in.close }
	}

	def touch(path: Path, log: Logger): Option[String] = touch(path.asFile, log)
	def touch(file: File, log: Logger): Option[String] =
	{
		Control.trapUnit("Could not create file " + printableFilename(file) + ": ", log)
		{
			if(file.exists)
				None
			else
				createDirectory(file.getParentFile, log) orElse { file.createNewFile(); None }
		}
	}
	def createDirectory(dir: Path, log: Logger): Option[String] = createDirectory(dir.asFile, log)
	def createDirectory(dir: File, log: Logger): Option[String] =
	{
		Control.trapUnit("Could not create directory " + printableFilename(dir) + ": ", log)
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
	}
	def createDirectories(d: Seq[Path], log: Logger): Option[String] = createDirectories(d.toList.map(_.asFile), log)
	def createDirectories(d: List[File], log: Logger): Option[String] =
		d match
		{
			case Nil => None
			case head :: tail => createDirectory(head, log) orElse createDirectories(tail, log)
		}
	private val MaximumTries = 10
	def createTemporaryDirectory(log: Logger): Either[String, File] =
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
			Control.trapAndFinally("", log)
				{ action(dir) }
				{ delete(dir, true, log) }
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
		creationError orElse ( Control.trapUnit("", log) { copy(sources.toList) } )
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
	
	def copyFilesFlat(sources: Iterable[File], targetDirectory: Path, log: Logger) =
	{
		require(targetDirectory.asFile.isDirectory)
		val byName = new scala.collection.mutable.HashMap[String, File]
		for(source <- sources) byName.put(source.getName, source)
		val uniquelyNamedSources = byName.values
		val targetSet = new scala.collection.mutable.HashSet[Path]
		def copy(source: File): Option[String] =
		{
			if(source.isDirectory)
				copyAll(source.listFiles.toList)
			else if(source.exists)
			{
				val targetPath = targetDirectory / source.getName
				targetSet += targetPath
				copyFile(source, targetPath.asFile, log)
			}
			else
				None
		}
		def copyAll(sources: List[File]): Option[String] =
			sources match
			{
				case head :: tail =>
					copy(head) match
					{
						case None => copyAll(tail)
						case x => x
					}
				case Nil => None
			}
		
		Control.trap("Error copying files: ", log) { copyAll(uniquelyNamedSources.toList).toLeft(targetSet.readOnly) }
	}
	def copyFile(sourceFile: Path, targetFile: Path, log: Logger): Option[String] =
		copyFile(sourceFile.asFile, targetFile.asFile, log)
	def copyFile(sourceFile: File, targetFile: File, log: Logger): Option[String] =
	{
		require(sourceFile.exists)
		require(!sourceFile.isDirectory)
		readChannel(sourceFile, log)(
			in => writeChannel(targetFile, log) {
				out => {
					val copied = out.transferFrom(in, 0, in.size)
					if(copied == in.size)
						None
					else
						Some("Could not copy '" + sourceFile + "' to '" + targetFile + "' (" + copied + "/" + in.size + " bytes copied)")
				}
			}
		)
	}
	
	def copyDirectory(source: Path, target: Path, log: Logger): Option[String] =
		copyDirectory(source.asFile, target.asFile, log)
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
		Control.trapUnit("Error deleting file " + printableFilename(file) + ": ", log)
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
	}
	
	private def open[T](file: File, log: Logger, constructor: File => T): Either[String, T] =
	{
		val parent = file.getParentFile
		if(parent != null)
			createDirectory(parent, log)
		Control.trap("Error opening " + printableFilename(file) + ": ", log) { Right(constructor(file)) }
	}
	private def urlInputStream(url: URL, log: Logger) =
		Control.trap("Error opening " + url + ": ", log) { Right(url.openStream) }
	private def fileOutputChannel(file: File, log: Logger) = open(file, log, (f: File) => (new FileOutputStream(f)).getChannel)
	private def fileInputChannel(file: File, log: Logger) = open(file, log, (f: File) => (new FileInputStream(f)).getChannel)
	private def fileInputStream(file: File, log: Logger) = open(file, log, (f: File) => new FileInputStream(f))
	private def fileOutputStream(append: Boolean)(file: File, log: Logger) =
		open(file, log, (f: File) => new FileOutputStream(f, append))
	private def fileWriter(charset: Charset, append: Boolean)(file: File, log: Logger) =
		open(file, log, (f: File) => new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f, append), charset)) )
	private def fileReader(charset: Charset)(file: File, log: Logger) =
		open(file, log, (f: File) => new BufferedReader(new InputStreamReader(new FileInputStream(f), charset)) )
	
	private def ioOption[Source, T <: Closeable](src: Source, open: (Source, Logger) => Either[String, T],
		f: T => Option[String], op: String, log: Logger): Option[String] =
			io(src, open, (t: T) => f(t).toLeft(()), op, log).left.toOption
	private def io[Source, T <: Closeable, R](src: Source, open: (Source, Logger) => Either[String, T],
		f: T => Either[String, R], op: String, log: Logger): Either[String, R] =
			open(src, log).right flatMap
			{
				stream => Control.trapAndFinally("Error " + op + src + ": ", log)
					{ f(stream) }
					{ stream.close }
			}
	
	def append(file: File, content: String, log: Logger): Option[String] = append(file, content, Charset.defaultCharset, log)
	def append(file: File, content: String, charset: Charset, log: Logger): Option[String] =
		write(file, content, charset, true, log)
	
	def write(file: File, content: String, log: Logger): Option[String] = write(file, content, Charset.defaultCharset, log)
	def write(file: File, content: String, charset: Charset, log: Logger): Option[String] =
		write(file, content, charset, false, log)
	private def write(file: File, content: String, charset: Charset, append: Boolean, log: Logger): Option[String] =
	{
		if(charset.newEncoder.canEncode(content))
			write(file, charset, append, log) { w =>  w.write(content); None }
		else
			Some("String cannot be encoded by default charset.")
	}
	
	def write(file: File, log: Logger)(f: Writer => Option[String]): Option[String] =
		write(file, Charset.defaultCharset, log)(f)
	def write(file: File, charset: Charset, log: Logger)(f: Writer => Option[String]): Option[String] =
		write(file, charset, false, log)(f)
	private def write(file: File, charset: Charset, append: Boolean, log: Logger)(f: Writer => Option[String]): Option[String] =
		ioOption(file, fileWriter(charset, append), f, "writing", log)
		
	def read(file: File, log: Logger)(f: Reader => Option[String]): Option[String] =
		unwrapEither(readValue(file, Charset.defaultCharset, log)(wrapEither(f)))
	def readValue[R](file: File, charset: Charset, log: Logger)(f: Reader => Either[String, R]): Either[String, R] =
		io(file, fileReader(charset), f, "reading", log)
		
	// error is in Left, value is in Right
	def readString(file: File, log: Logger): Either[String, String] = readString(file, Charset.defaultCharset, log)
	def readString(file: File, charset: Charset, log: Logger): Either[String, String] =
	{
		readValue(file, charset, log) {
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
	def appendBytes(file: File, bytes: Array[Byte], log: Logger): Option[String] =
		writeBytes(file, bytes, true, log)
	def writeBytes(file: File, bytes: Array[Byte], log: Logger): Option[String] =
		writeBytes(file, bytes, false, log)
	private def writeBytes(file: File, bytes: Array[Byte], append: Boolean, log: Logger): Option[String] =
		writeStream(file, append, log) { out => out.write(bytes); None }
	
	def readBytes(file: File, log: Logger): Either[String, Array[Byte]] =
		readStreamValue(file, log)
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
		
	def appendStream(file: File, log: Logger)(f: OutputStream => Option[String]): Option[String] =
		ioOption(file, fileOutputStream(true), f, "appending", log)
	def writeStream(file: File, log: Logger)(f: OutputStream => Option[String]): Option[String] =
		ioOption(file, fileOutputStream(false), f, "writing", log)
	private def writeStream(file: File, append: Boolean, log: Logger)(f: OutputStream => Option[String]): Option[String] =
		if(append) appendStream(file, log)(f) else writeStream(file, log)(f)
	def readStream(file: File, log: Logger)(f: InputStream => Option[String]): Option[String] =
		unwrapEither(readStreamValue(file, log)(wrapEither(f)))
	def readStreamValue[R](file: File, log: Logger)(f: InputStream => Either[String, R]): Either[String, R] =
		io(file, fileInputStream, f, "reading", log)
	def readStream(url: URL, log: Logger)(f: InputStream => Option[String]): Option[String] =
		unwrapEither(readStreamValue(url, log)(wrapEither(f)))
	def readStreamValue[R](url: URL, log: Logger)(f: InputStream => Either[String, R]): Either[String, R] =
		io(url, urlInputStream, f, "reading", log)
		
	def writeChannel(file: File, log: Logger)(f: FileChannel => Option[String]): Option[String] =
		ioOption(file, fileOutputChannel, f, "writing", log)
	def readChannel(file: File, log: Logger)(f: FileChannel => Option[String]): Option[String] =
		unwrapEither(readChannelValue(file, log)(wrapEither(f)))
	def readChannelValue[R](file: File, log: Logger)(f: FileChannel => Either[String, R]): Either[String, R] =
		io(file, fileInputChannel, f, "reading", log)
	
	private def wrapEither[R](f: R => Option[String]): (R => Either[String, Unit]) = (r: R) => f(r).toLeft(())
	private def unwrapEither(e: Either[String, Unit]): Option[String] = e.left.toOption
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