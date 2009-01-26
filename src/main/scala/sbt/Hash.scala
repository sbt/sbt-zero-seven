/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

object Hash
{
	final def toHex(bytes: Array[Byte]): String =
	{
		val buffer = new StringBuilder(bytes.length * 2)
		for(i <- 0 until bytes.length)
		{
			val b = bytes(i)
			val bi: Int = if(b < 0) b + 256 else b
			buffer append toHex((bi >>> 4).asInstanceOf[Byte])
			buffer append toHex((bi & 0x0F).asInstanceOf[Byte])
		}
		buffer.toString
	}
	final def fromHex(hex: String): Array[Byte] =
	{
		require((hex.length & 1) == 0, "Hex string must have length 2n.")
		val array = new Array[Byte](hex.length >> 1)
		for(i <- 0 until hex.length by 2)
		{
			val c1 = hex.charAt(i)
			val c2 = hex.charAt(i+1)
			array(i >> 1) = ((fromHex(c1) << 4) | fromHex(c2)).asInstanceOf[Byte]
		}
		array
	}
	final def apply(path: Path, log: Logger): Either[String, Array[Byte]] = apply(path.asFile, log)
	final def apply(file: java.io.File, log: Logger): Either[String, Array[Byte]] =
	{
		val BufferSize = 8192
		import java.security.{MessageDigest, DigestInputStream}
		val digest = MessageDigest.getInstance("SHA")
		FileUtilities.readStreamValue(file, log) { stream =>
			val dis = new DigestInputStream(stream, digest)
			val buffer = new Array[Byte](BufferSize)
			while(dis.read(buffer) >= 0) {}
			dis.close()
			Right(digest.digest)
		}
	}

	private def toHex(b: Byte): Char =
	{
		require(b >= 0 && b <= 15, "Byte " + b + " was not between 0 and 15")
		if(b < 10)
			('0'.asInstanceOf[Int] + b).asInstanceOf[Char]
		else
			('a'.asInstanceOf[Int] + (b-10)).asInstanceOf[Char]
	}
	private def fromHex(c: Char): Int =
	{
		val b =
			if(c >= '0' && c <= '9')
				(c - '0')
			else if(c >= 'a' && c <= 'f')
				(c - 'a') + 10
			else if(c >= 'A' && c <= 'F')
				(c - 'A') + 10
			else
				throw new RuntimeException("Invalid hex character: '" + c + "'.")
		b
	}
}