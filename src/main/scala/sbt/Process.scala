/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
 
/** The beginnings of a library for interacting with native processes.  It is not ready
 * for general use. */
package sbt

import java.lang.ProcessBuilder
import java.io.{File, InputStream, OutputStream}

class ProcessOptions(val workingDirectory: File, val redirectErrorStream: Boolean,
	val environment: Map[String, String], val useDefaultEnvironment: Boolean) extends NotNull
{
	def newDirectory(dir: File) = new ProcessOptions(dir, redirectErrorStream, environment, useDefaultEnvironment)
	def mergeErrorStream = new ProcessOptions(workingDirectory, true, environment, useDefaultEnvironment)
	def setEnvironment(env: Map[String, String]) =
		new ProcessOptions(workingDirectory, redirectErrorStream, env, true)
	def modifyEnvironment(modifiedMappings: Iterable[(String, String)]) = 
		new ProcessOptions(workingDirectory, redirectErrorStream, environment ++ modifiedMappings, useDefaultEnvironment)
}
class ProcessIO(val connectInput: OutputStream => Unit, val connectOutput: InputStream => Unit,
	val connectError: InputStream => Unit) extends NotNull
{
	def this() = this(ProcessIO.close, ProcessIO.readFully, ProcessIO.readFully)
	def this(log: Logger) = this(ProcessIO.close, ProcessIO.processFully(log, Level.Info), ProcessIO.processFully(log, Level.Error))
}
object ProcessIO
{
	import java.io.{BufferedReader, InputStreamReader}
	def close(c: java.io.Closeable) { c.close() }
	def readFully(i: InputStream) =
	{
		val buffer = new Array[Byte](8192)
		def readFully()
		{
			if(i.read(buffer) >= 0)
				readFully()
		}
		newThread(try { readFully() } finally { i.close() })
	}
	private def newThread(action: => Unit)
	{
		val runnable = new Runnable { def run() { action } }
		(new Thread(runnable)).start()
	}
	def processFully(log: Logger, level: Level.Value)(i: InputStream) { processFully(x => log.log(level, x))(i) }
	// processLine will be called from a new thread
	def processFully(processLine: String => Unit)(i: InputStream)
	{
		def readFully(reader: BufferedReader)
		{
			val line = reader.readLine()
			if(line != null)
			{
				processLine(line)
				readFully(reader)
			}
		}
		withReader(i)(readFully)
	}
	def fold[T](process: (T,String) => T, initial: T)(i: InputStream): T =
	{
		var v = initial
		processFully( line => v = process(v, line)  )(i)
		v
	}
	def standard: ProcessIO = standard(close)
	def standard(toProcessInput: OutputStream => Unit): ProcessIO =
		 new ProcessIO(toProcessInput, processFully(System.out.println), processFully(System.err.println))
	private def withReader(i: InputStream)(f: BufferedReader =>Unit) =
	{
		val reader = new BufferedReader(new InputStreamReader(i))
		newThread(try { f(reader) } finally { reader.close() })
	}
}
final class ProcessRunner(val command: String, val arguments: Seq[String], val options: ProcessOptions, val io: ProcessIO) extends NotNull
{
	require(!command.trim.isEmpty, "Command cannot be empty.")
	def this(command: String, arguments: Seq[String], workingDirectory: File, redirectErrorStream: Boolean) =
		this(command, arguments, new ProcessOptions(workingDirectory, redirectErrorStream, Map.empty, true), new ProcessIO)
	def this(command: String, arguments: Seq[String], workingDirectory: File) = this(command, arguments, workingDirectory, false)
	def this(command: String, arguments: Seq[String]) = this(command, arguments, new File("."))
	def this(command: String, argument0: String, arguments: String*) = this(command, argument0 :: arguments.toList, new File("."))
	def this(command: String) = this(command, Nil)
	
	def in(newDirectory: File) = new ProcessRunner(command, arguments, options.newDirectory(newDirectory), io)
	def withEnvironment(env: Map[String, String]) =
		new ProcessRunner(command, arguments, options.setEnvironment(env), io)
	def modifyEnvironment(modifiedMappings: (String, String)*) =
		new ProcessRunner(command, arguments, options.modifyEnvironment(modifiedMappings), io)
	def mergeErrorStream = new ProcessRunner(command, arguments, options.mergeErrorStream, io)
	def withIO(pio: ProcessIO) = new ProcessRunner(command, arguments, options, pio)
	def logIO(log: Logger) = withIO(new ProcessIO(log))
	
	def run: SProcess =
	{
		val commandArray = (command :: arguments.toList).toArray
		val builder = new ProcessBuilder(commandArray : _*)
		import options._
		builder.redirectErrorStream(redirectErrorStream).directory(workingDirectory)
		
		val env = builder.environment
		if(!useDefaultEnvironment)
			env.clear()
		for( (key, value) <- environment )
			env.put(key, value)
			
		val process = builder.start()
		import io._
		connectInput(process.getOutputStream)
		connectOutput(process.getInputStream)
		if(!options.redirectErrorStream)
			connectError(process.getErrorStream)
		new SProcess(process)
	}
	def commandLine = (command :: arguments.toList).mkString(" ")
}
final class SProcess(p: Process) extends NotNull
{
	def exitValue(): Int = p.waitFor()
	def destroy() { p.destroy }
}
