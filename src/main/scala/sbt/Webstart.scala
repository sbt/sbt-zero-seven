/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

import java.io.File
import scala.xml.{Elem, NodeSeq}
import Control._

trait WebstartOptions extends NotNull
{
	def webstartMainJar: Path
	def webstartOutputDirectory: Path
	def jnlpXML(jars: Seq[WebstartJarResource]): Elem
	def jnlpFile: Path
	def webstartLibDirectory: Path
	def webstartLibraries: PathFinder
	def webstartExtraLibraries: Iterable[File]
	def webstartResources: PathFinder
	def webstartZip: Option[Path]

	def webstartSignConfiguration: Option[SignConfiguration]
	def webstartPack200: Boolean
	def webstartGzip: Boolean
}
final case class WebstartJarResource(name: String, href: String, isMain: Boolean) extends NotNull
final case class SignConfiguration(alias: String, options: Seq[SignJar.SignOption]) extends NotNull
trait WebstartScalaProject extends ScalaProject
{
	private def writeXML(xmlString: String, outputPath: Path, log: Logger): Option[String] =
		FileUtilities.write(outputPath.asFile, xmlString, log)
	private def writeXML(xml: Elem, outputPath: Path, log: Logger): Option[String] =
	{
		val xmlString = scala.xml.Utility.toXML(xml, false)
		if(!outputPath.exists)
		{
			log.debug("JNLP file did not exist, writing inline XML to " + outputPath)
			writeXML(xmlString, outputPath, log)
		}
		else
		{
			val result =
				for( xmlHash <- Hash(xmlString, log).right; fileHash <- Hash(outputPath, log).right ) yield
				{
					if(xmlHash deepEquals fileHash)
					{
						log.debug("JNLP file " + outputPath + " uptodate.")
						None
					}
					else
					{
						log.debug("Inline JNLP XML modified, updating file " + outputPath + ".")
						writeXML(xmlString, outputPath, log)
					}
				}
			result.fold(err => Some(err), x => x)
		}
	}
	private def modifyExtension(jar: Path, newExtension: String, append: Boolean) =
		jar match
		{
			case rp: RelativePath =>
				import rp._
				if(append || !component.endsWith(".jar"))
					parentPath / (component + newExtension)
				else
					parentPath / ( component.substring(0, component.length - ".jar".length) + newExtension )
			case x => x
		}
	private def gzipJarPath(jar: Path) = modifyExtension(jar, ".gz", true)
	private def packPath(jar: Path) = modifyExtension(jar, ".pack", false)
	private def signOnly(jar: Path, signConfiguration: SignConfiguration, targetDirectory: Path) =
	{
		import SignJar._
		val targetJar = targetDirectory / jar.asFile.getName
		FileTasks.runOption("sign", targetJar from jar, log) {
			log.debug("Signing " + jar)
			(sign(jar, signConfiguration.alias, signedJar(targetJar.asFile.getAbsolutePath) :: signConfiguration.options.toList, log) orElse
				verify(jar, signConfiguration.options, log).map(err => "Signed jar failed verification: " + err))
		}.toLeft(targetJar :: Nil)
	}
	private def gzipJar(jar: Path) =
	{
		val gzipJar = gzipJarPath(jar)
		FileTasks.runOption("gzip", gzipJar from jar, log)
		{
			log.debug("Gzipping " + jar)
			FileUtilities.gzip(jar, gzipJar, log)
		}.toLeft(gzipJar :: Nil)
	}
	private def signAndPack200(jar: Path, signConfiguration: SignConfiguration, targetDirectory: Path) =
	{
		val signedJar = targetDirectory / jar.asFile.getName
		val packedJar = packPath(signedJar)
		import signConfiguration._
		
		FileTasks.runOption("sign and pack200", List(packedJar, signedJar) from jar, log) {
			log.debug("Applying pack200 compression and signing " + jar)
			signAndPack(jar, signedJar, packedJar, alias, options, log)
		}.toLeft(packedJar :: signedJar :: Nil)
	}
	private def signAndPack(jarPath: Path, signedPath: Path, out: Path, alias: String, options: Seq[SignJar.SignOption], log: Logger): Option[String] =
	{
		import Pack._
		import SignJar._
		pack(jarPath, out, log) orElse
		unpack(out, signedPath, log) orElse
		sign(signedPath, alias, options, log) orElse
		pack(signedPath, out, log) orElse
		unpack(out, signedPath, log) orElse
		verify(signedPath, options, log)
	}
	private def pack200Only(jar: Path, targetDirectory: Path) =
	{
		val targetJar = targetDirectory / jar.asFile.getName
		val packedJar = packPath(targetJar)
		val packResult =
			FileTasks.runOption("pack200", packedJar from jar, log)
			{
				log.debug("Applying pack200 compression to " + jar)
				Pack.pack(jar, packedJar, log)
			}
		packResult match
		{
			case Some(err) => Left(err)
			case None => copyJar(jar, targetDirectory).right.map(jars => packedJar :: jars)
		}
	}
	private def copyJar(jar: Path, targetDirectory: Path) =
	{
		val targetJar = targetDirectory / jar.asFile.getName
		FileTasks.runOption("copy jar", targetJar from jar, log)( FileUtilities.copyFile(jar, targetJar, log) ).toLeft(targetJar :: Nil)
	}
	private def isInDirectory(directory: Path, check: Path) = Path.relativize(directory, check).isDefined && directory != check
	def webstartTask(options: WebstartOptions) =
		task
		{
			import options._
			FileUtilities.createDirectories(webstartOutputDirectory :: webstartLibDirectory :: Nil, log) // ignore errors
			require(isInDirectory(webstartOutputDirectory, webstartLibDirectory), 
				"Webstart dependency directory (" + webstartLibDirectory + ") must be a subdirectory of webstart output directory (" +
					webstartOutputDirectory + ").")
			for(wz <- webstartZip)
				require(!isInDirectory(webstartOutputDirectory, wz),
					"Webstart output zip location (" + wz + " cannot be in webstart output directory (" + webstartOutputDirectory + ").")
		
			def relativize(jar: Path) = Path.relativize(webstartOutputDirectory ##, jar) getOrElse
				error("Jar (" + jar + ") was not in webstart output directory (" + webstartOutputDirectory + ").")
			def signAndPack(jars: List[Path], targetDirectory: Path): Either[String, List[Path]] =
			{
				lazyFold(jars, Nil: List[Path])
				{ (allJars, jar) =>
					log.debug("Processing jar " + jar)
					val signPackResult =
						webstartSignConfiguration match
						{
							case Some(config) =>
								if(webstartPack200)
									signAndPack200(jar, config, targetDirectory)
								else
									signOnly(jar, config, targetDirectory)
							case None =>
								if(webstartPack200)
									pack200Only(jar, targetDirectory)
								else
									copyJar(jar, targetDirectory)
						}
					signPackResult.right flatMap { addJars =>
						if(webstartGzip)
							Control.lazyFold(addJars, addJars ::: allJars) { (accumulate, jar) => gzipJar(jar).right.map(_ ::: accumulate) }
						else
							Right(addJars ::: allJars)
					}
				}
			}
		
			import FileUtilities._
			import ClasspathUtilities.isArchive
			
			def fileToPath(file: File): Path = new ProjectDirectory(file) // hack, don't do this normally
			val jars = (webstartLibraries.get ++ webstartExtraLibraries.map(fileToPath)).filter(isArchive)
			def process(jars: Iterable[Path]) = for(jar <- jars if jar.asFile.getName.endsWith(".jar")) yield relativize(jar)
			
			thread(signAndPack(webstartMainJar :: Nil, webstartOutputDirectory)) { mainJars =>
				thread(signAndPack(jars.toList, webstartLibDirectory)) { copiedJars =>
					val allJars = mainJars ::: copiedJars
					writeXML(jnlpXML(jarResources(process(mainJars), process(copiedJars))), jnlpFile, log) orElse
					thread(copy(webstartResources.get, webstartOutputDirectory, log)) { copiedResources =>
						val keep = jnlpFile +++ Path.lazyPathFinder(allJars ++ copiedResources) +++
							webstartOutputDirectory +++ webstartLibDirectory
						prune(webstartOutputDirectory, keep.get, log) orElse
						webstartZip.flatMap( zipPath => zip(List(webstartOutputDirectory ##), zipPath, true, log) )
					}
				}
			}
		}
		protected def defaultElements(resources: Seq[WebstartJarResource]): NodeSeq = resources.map(defaultElement)
		protected def defaultElement(resource: WebstartJarResource): Elem =
		{
			import resource._
			<jar href={href} main={isMain.toString}/>
		}
			
		private def jarResource(isMain: Boolean)(jar: Path): WebstartJarResource =
		{
			val file = jar.asFile
			WebstartJarResource(file.getName, jar.relativePath, isMain)
		}
		private def jarResources(mainJars: Iterable[Path], libraries: Iterable[Path]): Seq[WebstartJarResource] =
			mainJars.map(jarResource(true)).toList ::: libraries.map(jarResource(false)).toList
}
abstract class DefaultWebstartProject(val info: ProjectInfo) extends BasicWebstartProject
abstract class BasicWebstartProject extends BasicScalaProject with WebstartScalaProject with WebstartPaths
{
	def jnlpXML(libraries: Seq[WebstartJarResource]): Elem
	def webstartSignConfiguration: Option[SignConfiguration] = None
	
	def webstartExtraLibraries = scalaJars
	def webstartLibraries = runClasspath +++ jarsOfProjectDependencies
	def webstartResources = descendents(jnlpResourcesPath ##, AllPassFilter)

	def webstartPack200 = true
	def webstartGzip = true
	
	override def packageAction = super.packageAction && webstartTask(this)
}
trait WebstartPaths extends BasicProjectPaths with WebstartOptions
{
	def webstartOutputDirectory = outputPath / webstartDirectoryName
	def webstartMainJar = outputPath / defaultJarName
	
	def webstartLibDirectory = webstartOutputDirectory / webstartLibName
	def jnlpFile = webstartOutputDirectory / jnlpFileName
	def webstartZip: Option[Path] = Some(outputPath / webstartZipName)
	def jnlpPath = mainSourcePath / DefaultJnlpName
	def jnlpResourcesPath = jnlpPath / BasicProjectPaths.DefaultResourcesDirectoryName
	
	def jnlpFileName = DefaultJnlpFileName
	def webstartLibName = DefaultWebstartLibName
	def webstartDirectoryName = DefaultWebstartDirectoryName
	def webstartZipName = DefaultWebstartZipName
	
	val DefaultWebstartDirectoryName = "webstart"
	val DefaultJnlpName = "jnlp"
	val DefaultWebstartZipName = "webstart.zip"
	def DefaultJnlpFileName = defaultJarBaseName + ".jnlp"
	val DefaultWebstartLibName = "lib"
}