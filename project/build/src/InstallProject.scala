/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
import sbt._

import java.io.File
import java.nio.charset.Charset

protected class InstallPluginProject(info: ProjectInfo, extract: => InstallExtractProject) extends DefaultProject(info)
{
	private lazy val extractProject = extract
	override def crossScalaVersions = Set("2.7.2", "2.7.3", "2.7.4", "2.7.5")
	override def mainResources = super.mainResources +++ extractProject.outputJar +++ extractLocation
	
	def extractLocation = (outputPath ##) / "extract.location"
	lazy val writeProperties = task { FileUtilities.write(extractLocation.asFile, extractProject.outputJar.relativePath, Charset.forName("UTF-8"), log) }
	override def packageAction = super.packageAction dependsOn(extractProject.proguard, writeProperties)
}
protected class InstallExtractProject(info: ProjectInfo, pluginProject: => InstallPluginProject) extends DefaultProject(info)
{
	private lazy val plugin = pluginProject
	val mainClassName = "sbt.extract.Main"
	val proguardConfigurationPath: Path = "proguard.pro"
	val toolsConfig = config("tools")
	val proguardJar = "net.sf.proguard" % "proguard" % "4.3" % "tools->default"
	def rootProjectDirectory = rootProject.info.projectPath
	def outputJar = (plugin.outputPath ##) / defaultJarName
	def jarPath = outputPath / defaultJarName
	
	/******** Proguard *******/
	lazy val proguard = proguardTask dependsOn(`package`, writeProguardConfiguration)
	lazy val writeProguardConfiguration = writeProguardConfigurationTask dependsOn `package`
	
	private def proguardTask =
		task
		{
			FileUtilities.clean(outputJar :: Nil, log)
			val proguardClasspathString = Path.makeString(managedClasspath(toolsConfig).get)
			val configFile = proguardConfigurationPath.asFile.getAbsolutePath
			val exitValue = Process("java", List("-Xmx256M", "-cp", proguardClasspathString, "proguard.ProGuard", "@" + configFile)) ! log
			if(exitValue == 0) None else Some("Proguard failed with nonzero exit code (" + exitValue + ")")
		}
	private def writeProguardConfigurationTask =
		task
		{
			// the template for the proguard configuration file
			val outTemplate = """
				|-dontoptimize
				|-dontobfuscate
				|-dontnote
				|-dontwarn
				|-libraryjars %s
				|%s
				|-outjars %s
				|-ignorewarnings
				|-keep public class %s {
				|    public static void main(java.lang.String[]);
				|}"""
			
			val defaultJar = jarPath.absolutePath
			log.debug("proguard configuration using main jar " + defaultJar)
			val externalDependencies = (mainCompileConditional.analysis.allExternals).map(_.getAbsoluteFile).filter(_.getName.endsWith(".jar"))
			log.debug("proguard configuration external dependencies: \n\t" + externalDependencies.mkString("\n\t"))
			val projectDependencies = dependencies flatMap { case sc: BasicScalaProject => (sc.outputPath / sc.defaultJarName) :: Nil; case _ => Nil }
			// partition jars from the external jar dependencies of this project by whether they are located in the project directory
			// if they are, they are specified with -injars, otherwise they are specified with -libraryjars
			val (externalJars, libraryJars) = externalDependencies.toList.partition(jar => Path.relativize(rootProjectDirectory, jar).isDefined)
			log.debug("proguard configuration library jars locations: " + libraryJars.mkString(", "))
			val sbtJarStrings = projectDependencies.toList.map(_.absolutePath + "(!META-INF/**,!licenses/**,LICENSE,NOTICE,!*.xml)")
			val externalJarStrings = externalJars.map( _ + "(!META-INF/**,!*.properties)")
			// exclude properties files and manifests from scala-library jar
			val inJars = (defaultJar :: sbtJarStrings ::: externalJarStrings).map("-injars " + _).mkString("\n")
			
			val proguardConfiguration = outTemplate.stripMargin.format(libraryJars.mkString(File.pathSeparator), inJars, outputJar.absolutePath, mainClassName)
			log.debug("Proguard configuration written to " + proguardConfigurationPath)
			FileUtilities.write(proguardConfigurationPath.asFile, proguardConfiguration, log)
		}
}