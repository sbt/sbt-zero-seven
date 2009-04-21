import sbt._

import LoaderProject._

// a project for the sbt launcher
// the main content of this project definition is setting up and running proguard
//   to combine and compact all dependencies into a single jar
protected/* removes the ambiguity as to which project is the entry point by making this class non-public*/
	class LoaderProject(info: ProjectInfo) extends DefaultProject(info)
{
	val mainClassName = "sbt.boot.Boot"
	val baseName = "sbt-launcher"
	val proguardConfigurationPath: Path = "proguard.pro"
	val outputJar: Path = baseName + ".jar"
	
	override def mainClass = Some(mainClassName)
	override def defaultJarBaseName = baseName + "-" + version.toString
	
	def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
	override def mainResources = super.mainResources +++ extraResources
	
	val defaultConfig = config("default")
	val toolsConfig = config("tools")
	val ivy = "org.apache.ivy" % "ivy" % "2.0.0"
	val proguardJar = "net.sf.proguard" % "proguard" % "4.3" % "tools->default"
	
	lazy val proguard = proguardTask dependsOn(`package`, writeProguardConfiguration) describedAs(ProguardDescription)
	lazy val writeProguardConfiguration = writeProguardConfigurationTask dependsOn `package` describedAs WriteProguardDescription
	
	private def proguardTask =
		task
		{
			FileUtilities.clean(outputJar :: Nil, log)
			val proguardClasspath = managedClasspath(toolsConfig)
			val proguardClasspathString = Path.makeString(proguardClasspath.get)
			val configFile = proguardConfigurationPath.asFile.getAbsolutePath
			val p = new ProcessRunner("java", "-Xmx128M", "-cp", proguardClasspathString, "proguard.ProGuard", "@" + configFile).logIO(log)
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
				|-dontnote
				|-dontwarn
				|-libraryjars %s
				|-injars %s(!META-INF/**,!fr/**,!**/antlib.xml,!**/*.png)
				|%s
				|-outjars %s
				|-ignorewarnings
				|%s
				|-keep public class %s {
				|    public static void main(java.lang.String[]);
				|}"""
			
			val defaultJar = (outputPath / defaultJarName).asFile.getAbsolutePath
			log.debug("proguard configuration using main jar " + defaultJar)
			val ivyKeepOptions = ivyKeepResolvers.map("-keep public class " + _  + allPublic).mkString("\n")
			val externalDependencies = mainCompileConditional.analysis.allExternals.map(_.getAbsoluteFile).filter(_.getName.endsWith(".jar"))
			log.debug("proguard configuration external dependencies: \n\t" + externalDependencies.mkString("\n\t"))
			// pull out the Java rt.jar from the external jar dependencies of this project to put it in libraryjars
			val (rtJar, externalJars) = externalDependencies.toList.partition(_.getName == "rt.jar")
			log.debug("proguard configuration rt.jar location: " + rtJar.mkString(", "))
			// pull out Ivy in order to exclude resources inside
			val (ivyJars, otherExternalJars) = externalJars.partition(_.getName.startsWith("ivy"))
			log.debug("proguard configuration ivy jar location: " + ivyJars.mkString(", "))
			// exclude properties files and manifests from scala-library jar
			val inJars = (defaultJar :: otherExternalJars.map( _ + "(!META-INF/**,!*.properties)")).map("-injars " + _).mkString("\n")
			ivyJars match
			{
				case Nil => Some("Ivy not present (try running update)")
				case ivyJar :: _ =>
					val proguardConfiguration =
						outTemplate.stripMargin.format(rtJar.mkString, ivyJar.getAbsolutePath, inJars, outputJar, ivyKeepOptions, mainClassName)
					log.debug("Proguard configuration written to " + proguardConfigurationPath)
					FileUtilities.write(proguardConfigurationPath.asFile, proguardConfiguration, log)
			}
		}
	// class body declaration for proguard that keeps all public members
	private val allPublic = " {\n public * ;\n}"
}
object LoaderProject
{
	val ProguardDescription = "Produces the final compacted jar that contains only the minimum classes needed using proguard."
	val WriteProguardDescription = "Creates the configuration file to use with proguard."
}