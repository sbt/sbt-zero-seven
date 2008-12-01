/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

sealed trait Version extends NotNull
case class BasicVersion(major: Int, minor: Option[Int], micro: Option[Int], extra: Option[String]) extends Version
{
	override def toString = major +
		minor.map(minorI => "." + minorI + micro.map(microI => "." + microI).getOrElse("")).getOrElse("") +
			extra.map(x => "-" + x).getOrElse("")
}
case class OpaqueVersion(value: String) extends Version
{
	override def toString = value
}
object Version
{
	import java.util.regex.Pattern
	val versionPattern = Pattern.compile("""(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:-(.+))?""")
	def fromString(v: String): Either[String, Version] =
	{
		val trimmed = v.trim
		if(trimmed.isEmpty)
			Left("Version cannot be empty.")
		else
		{
			val matcher = versionPattern.matcher(trimmed)
			import matcher._
			if(matches)
			{
				def toOption(index: Int) =
				{
					val v = group(index)
					if(v == null) None else Some(v)
				}
				def toInt(index: Int) =
				{
					val v = group(index)
					if(v == null) None else Some(v.toInt)
				}
				Right(BasicVersion(group(1).toInt, toInt(2), toInt(3), toOption(4)))
			}
			else
				Right(OpaqueVersion(trimmed))
		}
	}
}
