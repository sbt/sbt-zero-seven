package sbt

import java.io.{BufferedReader, File, InputStreamReader, PrintStream, Reader}

object TestedProcess
{
	val ReadLines = "read-lines"
	val WriteOut = "write-out"
	val WriteError = "write-err"
	val ReadWriteLine = "read-write-line"
	val ReadWriteErrorLine = "read-write-error-line"
	val Exit = "exit"
	val OutLine = "test-out"
	val ErrorLine = "test-error"
	def main(args: Array[String])
	{
		val inputReader = new BufferedReader(new InputStreamReader(System.in))
		try { exit(run(inputReader, args.toList)) }
		finally { inputReader.close() }
	}
	private def run(input: BufferedReader, args: List[String]): Int =
		args match
		{
			case Nil => 0
			case arg :: tail =>
				run(input, arg) match
				{
					case None => run(input, tail)
					case Some(exitCode) => exitCode
				}
		}
	private def run(input: BufferedReader, arg: String): Option[Int] =
	{
		val split = arg.split("=").map(_.trim)
		if(split.length == 2)
		{
			val Array(command, value) = split
			val num = value.toInt
			run(input, command, num)
		}
		else
			error("Expected key=value, got '" + arg + "'")
	}
	private def run(input: BufferedReader, command: String, num: Int): Option[Int] =
	{
		command match
		{
			case ReadLines => readLines(input, num)(x => ()); None
			case WriteOut => print(System.out, OutLine, num); None
			case WriteError => print(System.err, ErrorLine, num); None
			case ReadWriteLine => readLines(input, num)(System.out.println); None
			case ReadWriteErrorLine => readLines(input, num)(System.err.println); None
			case Exit => Some(num)
			case x => None
		}
	}
	private def print(out: PrintStream, line: String, times: Int)
	{
		if(times > 0)
		{
			out.println(line)
			print(out, line, times - 1)
		}
	}
	private def readLines(input: BufferedReader, count: Int)(f: String => Unit)
	{
		if(count > 0)
		{
			val line = input.readLine()
			if(line == null) ""
			else
			{
				f(line)
				readLines(input, count-1)(f)
			}
		}
	}
}