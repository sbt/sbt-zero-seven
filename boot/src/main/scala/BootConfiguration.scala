/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
 package sbt

// project/boot/                [BootDirectoryName]
//     scala-<version>/    [baseDirectoryName]
//          lib/                      [ScalaDirectoryName]
//          sbt-<version>/  [sbtDirectoryName]

private[sbt] object BootConfiguration
{
	val ScalaOrg = "org.scala-lang"
	val SbtOrg = "sbt"
	val CompilerModuleName = "scala-compiler"
	val SbtModuleName = "simple-build-tool"
	val ConflictManagerName = "strict"
	val SbtMainClass = "sbt.Main"
	val ScalaVersion = "scala.version"
	val SbtVersion = "sbt.version"
	val ScalaPackage = "scala."
	val IvyPackage = "org.apache.ivy."
	
	val ProjectDirectoryName = "project"
	val BootDirectoryName = "boot"
	val BuildPropertiesName ="build.properties"
	val ScalaHomeProperty = "scala.home"
	val UpdateLogName = "update.log"
	
	val RebootCommand = "reboot"
	val MainMethodName = "main"
	val DefaultIvyConfiguration = "default"
	
	val sbtRootBase = "http://simple-build-tool.googlecode.com/svn/artifacts/"
	val ScalaDirectoryName = "lib"
	val scalaRetrievePattern = ScalaDirectoryName + "/[artifact].[ext]"
	
	def sbtRetrievePattern(sbtVersion: String) = sbtDirectoryName(sbtVersion) + "/[artifact]-[revision].[ext]"
	def sbtResolverPattern(scalaVersion: String) = sbtRootBase + "[revision]/[type]s/[artifact].[ext]"
	def sbtDirectoryName(sbtVersion: String) = SbtOrg + "-" + sbtVersion
	def baseDirectoryName(scalaVersion: String) = "scala-" + scalaVersion
}