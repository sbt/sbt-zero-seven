package sbt

sealed trait Version extends NotNull
case class BasicVersion(major: Int, minor: Option[Int], micro: Option[Int], build: Option[Int], status: Option[String]) extends Version
{
	override def toString = major +
		minor.map(minorI => "." + minorI + micro.map(microI => "." + microI).getOrElse("")).getOrElse("") +
		build.map("-b" + _).getOrElse("") +
		status.map("-" + _).getOrElse("")
}
case class OpaqueVersion(value: String) extends Version
{
	override def toString = value
}
object Version
{
	import java.util.regex.Pattern
	val versionPattern = Pattern.compile("""(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:-b(\d+))?(?:-(\.+))?""")
	def fromString(v: String): Either[String, Version] =
	{
		val trimmed = v.trim
		if(trimmed.isEmpty)
			Left("No project version specified")
		else if(trimmed.charAt(0) == '"')
		{
			if(trimmed.length == 1)
				Left("Closing \" not found")
			else if(trimmed.charAt(trimmed.length - 1) != '"')
				Left("Opaque version must end with \"")
			else
				Right(OpaqueVersion(trimmed))
		}
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
				Right(BasicVersion(group(1).toInt, toInt(2), toInt(3), toInt(4), toOption(5)))
			}
			else
				Left("Invalid version string " + v + ", expected #[.#[.#]][-b#][-*]")
		}
	}
}
