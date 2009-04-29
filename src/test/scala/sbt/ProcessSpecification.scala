package sbt

import java.io.{File, InputStream, OutputStream, OutputStreamWriter, PrintWriter}
import org.scalacheck.Properties

import TestedProcess._
/* TODO: rewrite for new process code
object ProcessSpecification extends Properties("Process I/O")
{
	specify("Correct exit code", (exitCode: Byte) => checkExit(exitCode))
	specify("Process reads standard in", (times: Int, line: String) => writeToProcess(Math.abs(times), strip(line)))
	specify("Process writes standard out", (times: Int) => readProcess(Math.abs(times), Some(OutLine), None))
	specify("Process writes standard error", (times: Int) => readProcess(Math.abs(times), None, Some(ErrorLine)))
	specify("Process writes standard out and error", (times: Int) => readProcess(Math.abs(times), Some(OutLine), Some(ErrorLine)))
	
	private def checkExit(code: Byte) =
	{
		val exitCode = unsigned(code)
		run(keyValue(Exit, exitCode) :: Nil, exitCode)
	}

	private def readProcess(count: Int, lineOut: Option[String], lineErr: Option[String]) =
	{
		def read(line: Option[String]) =
			line match
			{
				case None => ProcessIO.processFully(System.out.println)_
				case Some(l) => readLines(List.make(count, l))
			}
		def arg(key: String, include: Boolean) =
			if(include) keyValue(key,count) :: Nil
			else Nil
		val args = arg(WriteOut, lineOut.isDefined) ::: arg(WriteError, lineErr.isDefined)
		setupRun(args, 0)
			{ _.withIO(new ProcessIO(ProcessIO.close, read(lineOut), read(lineErr))) }
	}
	private def keyValue(key: String, value: Int) = key + "=" + value
	private def strip(line: String) = line.replaceAll("[\n\r]","")
	private def writeToProcess(count: Int, line: String) =
	{
		setupRun(keyValue(TestedProcess.ReadLines, count) :: Nil, 0)
			{ _.withIO(ProcessIO.standard(writeLines(line, count))) }
	}
	private def writeLines(line: String, times: Int)(o: OutputStream)
	{
		val out = new PrintWriter(new OutputStreamWriter(o))
		def writeLines(remaining: Int)
		{
			if(remaining > 0)
			{
				out.println(line)
				writeLines(remaining - 1)
			}
		}
		try { writeLines(if(line.isEmpty) 0 else times) }
		finally { out.close() }
	}

	private def unsigned(b: Byte): Int = ((b: Int) +256) % 256
	private def run(args: List[String], expectedExitCode: Int) = setupRun(args, expectedExitCode)(s => s)
	private def setupRun(args: List[String], expectedExitCode: Int)(setup: ProcessRunner => ProcessRunner) =
	{
		val ignore = TestedProcess // just for the compile dependency so that this test is rerun when TestedProcess changes, not used otherwise
		
		val thisClasspath = List(getSource[ScalaObject], getSource[sbt.SourceTag]).mkString(File.pathSeparator)
		val runner = setup(Process("java", "-cp" :: thisClasspath :: "sbt.TestedProcess" :: args))
		val exitCode = runner !
		exitCode == expectedExitCode
	}
	private def getSource[T](implicit mf: scala.reflect.Manifest[T]): String =
		(new File(mf.erasure.getProtectionDomain.getCodeSource.getLocation.toURI)).getAbsolutePath
		
	private def readLines(expected: List[String]): InputStream => Unit =
	{
		def next(errorOrRemaining: Either[String, List[String]], line: String) =
		{
			errorOrRemaining.right.flatMap
			{
				_ match
				{
					case Nil => Left("Expected end of input, got '" + line + "'")
					case head :: tail =>
						if(head == line)
							Right(tail)
						else
							Left("Expected '" + head + "', got '" + line+ "'")
				}
			}
		}
		ProcessIO.fold(next, Right(expected))
	}
}
private trait SourceTag*/