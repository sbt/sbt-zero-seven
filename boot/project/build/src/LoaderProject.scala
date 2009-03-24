package sbt

import sbt._

import LoaderProject._
// a project for the sbt launcher
// the main content of this project definition is setting up and running proguard
//   to combine and compact all dependencies into a single jar
class LoaderProject(info: ProjectInfo) extends DefaultProject(info)
{
	val mainClassName = "sbt.boot.Boot"
	val baseName = "sbt-loader"
	val proguardConfigurationPath: Path = "proguard.pro"
	val outputJar: Path = baseName + ".jar"
	
	override def mainClass = Some(mainClassName)
	override def defaultJarBaseName = baseName + "-" + version.toString
	
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	
	lazy val proguard = proguardTask dependsOn(`package`, writeProguardConfiguration) describedAs(ProguardDescription)
	lazy val writeProguardConfiguration = writeProguardConfigurationTask dependsOn `package` describedAs WriteProguardDescription
	
	private def proguardTask =
		task
		{
			FileUtilities.clean(outputJar :: Nil, log)
			val p = new ProcessRunner("java", "-Xmx128M", "-jar", "proguard.jar", "@" + proguardConfigurationPath).logIO(log)
			val exitValue = p.run.exitValue
			if(exitValue == 0) None else Some("Proguard failed with nonzero exit code (" + exitValue + ")")
		}
	private def writeProguardConfigurationTask =
		task
		{
			// these are classes that need to be explicitly kept because they are loaded reflectively
			val ivyKeepResolvers = "org.apache.ivy.plugins.resolver.URLResolver" :: "org.apache.ivy.plugins.resolver.IBiblioResolver" :: Nil
			// the template for the proguard configuration file
			val outTemplate = """
				|-dontoptimize
				|-dontobfuscate
				|-libraryjars %s
				|-libraryjars scala-bug-1572-workaround.jar
				|-injars %s(!META-INF/**,!fr/**,!**/antlib.xml,!**/*.png)
				|%s
				|-outjars %s
				|-ignorewarnings
				|%s
				|-keep public class %s {
				|    public static void main(java.lang.String[]);
				|}"""
			val scalaWorkaround = "scala-bug-1572-workaround.jar".asFile
			
			val defaultJar = (outputPath / defaultJarName).toString
			// pull out the Java rt.jar from the external jar dependencies of this project to put it in libraryjars
			val (rtJar, externalJars) = mainCompileConditional.analysis.allExternals.filter(ClasspathUtilities.isArchive).toList.partition(jar => jar.getName == "rt.jar")
			// pull out Ivy in order to exclude resources inside
			val (ivyJar, otherExternalJars) = externalJars.partition(jar => jar.getName.startsWith("ivy"))
			// exclude properties files and manifests from scala-library jar
			val inJars = (defaultJar :: otherExternalJars.map( _ + "(!META-INF,!*.properties)")).map("-injars " + _).mkString("\n")
			val ivyKeepOptions = ivyKeepResolvers.map("-keep public class " + _  + allPublic).mkString("\n")
			val proguardConfiguration = outTemplate.stripMargin.format(rtJar.mkString, ivyJar.mkString, inJars, outputJar, ivyKeepOptions, mainClassName)
			FileUtilities.write(proguardConfigurationPath.asFile, proguardConfiguration, log)
		}
	// class body declaration for proguard that keeps all public members
	private val allPublic = " {\n public * ;\n}"
}
object LoaderProject
{
	val ProguardDescription = "Produces the final compacted jar that contains only the minimum classes needed using proguard."
	val WriteProguardDescription = "Creates the configuration file to use with proguard."
}