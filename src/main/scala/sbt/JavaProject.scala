/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

trait JavaProject extends BasicScalaProject with JavaPaths
{
	def mainJavaSources = descendents(mainJavaSourcePath, "*.java")
	override def mainSources = mainScalaSources +++ mainJavaSources
}
trait JavaPaths extends BasicProjectPaths
{
	import JavaProject._
	
	def javaDirectoryName = DefaultJavaDirectoryName
	def mainJavaSourcePath = mainSourcePath / javaDirectoryName
}
object JavaProject
{
	val DefaultJavaDirectoryName = "java"
}