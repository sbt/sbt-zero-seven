package sbt

import sbt._

class LoaderProject(info: ProjectInfo) extends DefaultProject(info)
{
	val mainClassName = "sbt.Boot"
	val baseName = "sbt-loader"
	val proguardConfigurationPath: Path = "proguard.pro"
	val outputJar: Path = baseName + ".jar"
	
	override def mainClass = Some(mainClassName)
	override def defaultJarBaseName = baseName + "-" + version.toString
	
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	
	lazy val proguard = proguardTask dependsOn(`package`, writeProguardConfiguration)
	lazy val writeProguardConfiguration = writeProguardConfigurationTask dependsOn `package`
	
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
			val ivyKeepResolvers = "org.apache.ivy.plugins.resolver.URLResolver" :: "org.apache.ivy.plugins.resolver.IBiblioResolver" :: Nil
			val outTemplate = """
				|-dontoptimize
				|-dontobfuscate
				|-libraryjars %s
				|-libraryjars scala-bug-1572-workaround.jar
				|-injars %s(!META-INF,!fr/**,!**/antlib.xml,!**/*.png)
				|%s
				|-outjars %s
				|-ignorewarnings
				|%s
				|-keep public class %s {
				|    public static void main(java.lang.String[]);
				|}"""
			val scalaWorkaround = "scala-bug-1572-workaround.jar".asFile
			val (rtJar, externalJars) = mainCompileConditional.analysis.allExternals.filter(ClasspathUtilities.isArchive).toList.partition(jar => jar.getName == "rt.jar")
			val defaultJar = (outputPath / defaultJarName).toString
			val (ivyJar, otherExternalJars) = externalJars.partition(jar => jar.getName.startsWith("ivy"))
			val inJars = (defaultJar :: otherExternalJars.map( _ + "(!META-INF,!*.properties)")).map("-injars " + _).mkString("\n")
			val ivyKeepOptions = ivyKeepResolvers.map("-keep public class " + _  + allPublic).mkString("\n")
			val proguardConfiguration = outTemplate.stripMargin.format(rtJar.mkString, ivyJar.mkString, inJars, outputJar, ivyKeepOptions, mainClassName)
			FileUtilities.write(proguardConfigurationPath.asFile, proguardConfiguration, log)
		}
	private val allPublic = " {\n public * ;\n}"
}