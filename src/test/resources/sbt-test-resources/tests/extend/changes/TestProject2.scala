import sbt._

class TestProject2(info: ProjectInfo) extends DefaultProject(info)
{
	override def testFrameworks = framework.FrameworkScalaCheck :: Nil
	
	val sc = "org.scala-tools.testing" % "scalacheck" % "1.5" % "test->default"
}