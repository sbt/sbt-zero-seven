import sbt._

import java.io.File
import scala.xml.NodeSeq

/** Support for compiling sbt across multiple versions of Scala.  The scala compiler is run in a
* separate JVM and no partial compilation is done.*/
abstract class CrossCompileProject extends BasicScalaProject
{
	/** Used for 2.8.0-SNAPSHOT*/
	val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots"

	/* The base configuration names for the versions of Scala*/
	private val version2_7_2 = "2.7.2"
	private val version2_7_3 = "2.7.3"
	private val version2_7_4 = "2.7.4"
	private val version2_8_0 = "2.8.0-SNAPSHOT"
	private val base = "base"

	private def developmentVersion = version2_7_2

	/* The configurations for the versions of Scala.*/
	private val conf_2_7_2 = config(version2_7_2)
	private val conf_2_7_3 = config(version2_7_3)
	private val conf_2_7_4 = config(version2_7_4)
	private val conf_2_8_0 = config(version2_8_0)
	// the list of all configurations to cross-compile against
	private val allConfigurations = conf_2_7_2 :: conf_2_7_3 :: conf_2_7_4 :: conf_2_8_0 :: Nil
	// the names of all configurations to cross-compile against
	private val allConfigurationsNames = allConfigurations.map(_.toString)

	/* Methods to derive the configuration name from the base name 'v'.*/
	private def optional(v: String) = "optional-" + v
	private def scalac(v: String) = "scalac-" + v
	private def sbt(v: String) = "sbt_" + v
	private def depConf(v: String) = v + "->default"
	
	// =========== Cross-compilation across scala versions ===========
	
	// The dependencies that should go in each configuration are:
	//   base                             Required dependencies that are the same across all scala versions.
	//   <version>                  Required dependencies to use with Scala <version>
	//   optional-base              Optional dependencies that are the same for all scala versions
	//   optional-<version>   Optional dependencies to use with Scala <version>
	//   compile                        Used for normal development, it should extend a specific <version> and optional-<version>
	//   scalac-<version>       The scala compiler for Scala <version>
	// There should be a jar publication for each version of scala.  The artifact should be named sbt_<version>.
	override def ivyXML =
		(<configurations>
			<conf name={base}/>
			<conf name={optional(base)}/>
			{ variableConfigurations }
			<!-- The configuration used for normal development (actions other than cross-*) -->
			<conf name="default" extends={developmentVersion + "," + optional(developmentVersion)} visibility="private"/>
		</configurations>
		<publications>
			{ publications }
		</publications>
		<dependencies>
			<!-- Dependencies that are the same across all Scala versions -->
			<dependency org="org.apache.ivy" name="ivy" rev="2.0.0" transitive="false" conf={depConf(base)}/>
			{testDependency("scalacheck", "1.5", false, base)}
			<dependency org="org.mortbay.jetty" name="jetty" rev="6.1.14" transitive="true" conf={depConf(optional(base))}/>

			<!-- the dependencies that are different dependeding on the version of Scala -->
			{ variableDependencies(version2_7_2, /*ScalaTest*/"0.9.3", /*Specs*/"1.4.0", false) }
			{ variableDependencies(version2_7_3, /*ScalaTest*/"0.9.4", /*Specs*/"1.4.3", true) }
			{ variableDependencies(version2_7_4, /*ScalaTest*/"0.9.5", /*Specs*/"1.4.3", true) }
			{ variableDependencies(version2_8_0, /*ScalaTest*/"0.9.5", /*Specs*/"1.4.3", true) }
		</dependencies>)

	/** Creates a publication (an 'artifact' element) for each Scala version */
	private def publications: NodeSeq =
	{
		for(conf <- allConfigurationsNames) yield
			<artifact name={sbt(conf)} conf={conf}/>
	}
	/** Creates the main, optional, and scalac configurations for each Scala version*/
	private def variableConfigurations: NodeSeq =
	{
		allConfigurationsNames flatMap
		{ conf =>
			scalaComment(conf) ++
			(<conf name={conf} extends={base}/>
			<conf name={optional(conf)} extends={optional(base)}/>
			<conf name={scalac(conf)} visibility="private"/>)
		}
	}
	/** Defines the dependencies for the given version of Scala, ScalaTest, and Specs.  If uniformTestOrg is true,
	* the 'org.scala-tools.testing' organization is used.  Otherwise, 'org.' is prefixed to the module name. */
	private def variableDependencies(scalaVersion: String, scalaTestVersion: String, specsVersion: String, uniformTestOrg: Boolean) =
	{
		scalaComment(scalaVersion) ++
		testDependency("scalatest", scalaTestVersion, uniformTestOrg, scalaVersion) ++
		testDependency("specs", specsVersion, uniformTestOrg, scalaVersion) ++
		<dependency org="org.scala-lang" name="scala-compiler" rev={scalaVersion} conf={depConf(scalac(scalaVersion))}/>
	}
	/** Creates a comment containing the version of Scala*/
	private def scalaComment(scalaVersion: String) = scala.xml.Comment("Scala " + scalaVersion)
	/** Creates a dependency element for a test.  See 'testOrg' for a description of uniformTestOrg.*/

	private def testDependency(name: String, version: String, uniformTestOrg: Boolean, baseConf: String) =
		<dependency org={testOrg(name, uniformTestOrg)} name={name} rev={version} transitive="false" conf={depConf(optional(baseConf))}/>
		
	/** Returns the organization for the given test library.  If uniform is true,
	* the 'org.scala-tools.testing' organization is used.  Otherwise, 'org.' is prefixed to the module name.*/
	private def testOrg(name: String, uniform: Boolean) =
		if(uniform) "org.scala-tools.testing"
		else "org." + name

	/** Disable filtering Scala jars from dependency management, because we need them and are putting them
	* in custom configurations and are using them in a separate process than sbt runs in.*/
	override def filterScalaJars = false
	
	/** The lib directory is now only for building using the 'build' script.*/
	override def unmanagedClasspath = path("ignore_lib_directory")
	/** When cross-compiling, replace mainCompilePath with the classes directory for the version being compiled.*/
	override def fullUnmanagedClasspath(config: Configuration) =
		if( (Configurations.Default :: Configurations.defaultMavenConfigurations) contains config)
			super.fullUnmanagedClasspath(config)
		else
			classesPath(config.toString) +++ mainResourcesPath
	
	// include the optional-<version> dependencies as well as the ones common across all scala versions
	def optionalClasspath(version: String) = fullClasspath(config(optional(version))) +++ super.optionalClasspath
	
	private val CompilerMainClass = "scala.tools.nsc.Main"
	// use a publish configuration that publishes the 'base' + all <version> configurations (base is required because
	//   the <version> configurations extend it)
	private val conf = new DefaultPublishConfiguration("local", "release")
	{
		override def configurations: Option[Iterable[Configuration]] = Some(config(base) :: allConfigurations)
	}
	// the actions for cross-version packaging and publishing
	lazy val crossPackage = allConfigurations.map(conf => packageForScala(conf.toString))
	lazy val crossDeliverLocal = deliverTask(conf, updateOptions) dependsOn(crossPackage : _*)
	lazy val crossPublishLocal = publishTask(conf, updateOptions) dependsOn(crossDeliverLocal)
	// Creates a task that produces a packaged sbt compiled against Scala scalaVersion.
	//  The jar is named 'sbt_<scala-version>-<sbt-version>.jar'
	private def packageForScala(scalaVersion: String) =
	{
		val classes = classesPath(scalaVersion) ** "*"
		val jarName = crossJarName(scalaVersion)
		packageTask(classes +++ mainResources, outputPath, jarName, packageOptions).dependsOn(compileForScala(scalaVersion))
	}
	private def crossJarName(scalaVersion: String) = sbt(scalaVersion) + "-" + version.toString +  ".jar"
	// This creates a task that compiles sbt against the given version of scala.  Classes are put in classes-<scalaVersion>.
	private def compileForScala(version: String)=
		task
		{
			val classes = classesPath(version)
			val toClean = (outputPath / crossJarName(version)) +++ (classes ** "*")
			val setupResult =
				FileUtilities.clean(toClean.get, true, log) orElse
				FileUtilities.createDirectory(classes, log)
			for(err <- setupResult) log.error(err)
			// the classpath containing the scalac compiler
			val compilerClasspath = concatPaths(fullClasspath(config(scalac(version))))
			
			// The libraries to compile sbt against
			val classpath = fullClasspath(config(version)) +++ optionalClasspath(version)
			val sources: List[String] = pathListStrings(mainSources.get)
			val compilerOptions = List("-cp", concatPaths(classpath), "-d", classes.toString)
			val compilerArguments: List[String] = compilerOptions ::: sources
			
			// the compiler classpath has to be appended to the boot classpath to work properly
			val allArguments = "-Xmx256M" :: ("-Xbootclasspath/a:" + compilerClasspath) :: CompilerMainClass :: compilerArguments
			log.debug("Running external compiler with command: java " + allArguments.mkString(" "))
			val exitValue = Process("java", allArguments) ! log
			if(exitValue == 0)
				None
			else
				Some("Nonzero exit value (" + exitValue + ") when calling scalac " + version + " with options: \n" + compilerOptions.mkString(" "))
		}
	private def concatPaths(p: PathFinder): String = pathListStrings(p.get).mkString(File.pathSeparator)
	private def pathListStrings(p: Iterable[Path]): List[String] = p.map(_.asFile.getAbsolutePath).toList
	private def classesPath(scalaVersion: String) = ("target"  / ("classes-" + scalaVersion)) ##
}