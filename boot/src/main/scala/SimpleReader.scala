package sbt.boot

import jline.ConsoleReader
object SimpleReader extends NotNull
{
	protected[this] val reader =
	{
		val cr = new ConsoleReader
		cr.setBellEnabled(false)
		cr
	}
	def readLine(prompt: String) =
		reader.readLine(prompt) match
		{
			case null => None
			case x =>
				val trimmed = x.trim
				if(trimmed.isEmpty)
					None
				else
					Some(trimmed)
		}
}