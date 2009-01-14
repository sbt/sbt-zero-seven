/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import org.scalacheck._

object WriteContentSpecification extends Properties("Write content")
{
	val log = new ConsoleLogger
	log.setLevel(Level.Warn)
	
	specify("Roundtrip string", writeAndCheckString _)
	specify("Roundtrip bytes", writeAndCheckBytes _)

	import FileUtilities._
	private def writeAndCheckString(s: String) =
	{
		// make the test independent of underlying platform and allow any unicode character to be encoded
		val charset = java.nio.charset.Charset.forName("UTF-8")
		val result =
			doInTemporaryDirectory(log) { dir =>
				val file = new java.io.File(dir, "out")
				write(file, s, charset, log).toLeft(()).right.flatMap { x => readString(file, charset, log) }
			}
		handleResult[String](result, _ == s)
	}
	private def writeAndCheckBytes(b: Array[Byte]) =
	{
		val result =
			doInTemporaryDirectory(log) { dir =>
				val file = new java.io.File(dir, "out")
				write(file, b, log).toLeft(()).right.flatMap { x => readBytes(file, log) }
			}
		handleResult[Array[Byte]](result, _ deepEquals b)
	}
	private def handleResult[T](result: Either[String, T], check: T => Boolean): Boolean =
		result match
		{
			case Left(err) => log.trace(new RuntimeException(err)); log.error(err); false
			case Right(x) => check(x)
		}
	
}