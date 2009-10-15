import sbt._

class TestProject(info: ProjectInfo) extends DefaultProject(info)
{
	val specs = "org.specs" % "specs" % "1.4.1"
}