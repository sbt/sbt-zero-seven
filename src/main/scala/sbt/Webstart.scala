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
	protected def writeXML(xml: Elem, outputPath: Path, log: Logger): Option[String] =
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
	protected def modifyExtension(jar: Path, newExtension: String, append: Boolean) =
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
	protected def gzipJarPath(jar: Path) = modifyExtension(jar, ".gz", true)
	protected def packPath(jar: Path) = modifyExtension(jar, ".pack", false)
	protected def signOnly(jar: Path, signConfiguration: SignConfiguration) =
	{
		// TODO: only run if not signed
		SignJar.sign(jar, signConfiguration.alias, signConfiguration.options, log) orElse
			SignJar.verify(jar, signConfiguration.options, log).map(err => "Signed jar failed verification: " + err)
	}
	protected def gzipJar(jar: Path) =
	{
		val gzipJar = gzipJarPath(jar)
		FileTasks.runOption("gzip", gzipJar from jar, log)( FileUtilities.gzip(jar, gzipJar, log) )
	}
	protected def signAndPack200(jar: Path, signConfiguration: SignConfiguration) =
	{
		val packedJar = packPath(jar)
		import signConfiguration._
		FileTasks.runOption("sign and pack200", packedJar from jar, log)( Pack.signAndPack(jar, packedJar, alias, options, log) )
	}
	protected def pack200Only(jar: Path) =
	{
		val packedJar = packPath(jar)
		FileTasks.runOption("pack200", packedJar from jar, log)( Pack.pack(jar, packedJar, log) )
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
		
			val outputJar = webstartOutputDirectory / webstartMainJar.asFile.getName
	
			def relativize(jar: Path) = Path.relativize(webstartOutputDirectory ##, jar) getOrElse
				error("Jar (" + jar + ") was not in webstart output directory (" + webstartOutputDirectory + ").")
			def signAndPack(jars: List[Path]): Either[String, List[Path]] =
			{
				log.debug(jars.mkString("Processing jars ", " ", ""))
				lazyFold(jars, Nil: List[Path])
				{ (allJars, jar) =>
					def jarAndPacked(o: Option[String]) = o.toLeft(jar :: packPath(jar) :: Nil)
					val signPackResult =
						webstartSignConfiguration match
						{
							case Some(config) =>
								if(webstartPack200)
									jarAndPacked(signAndPack200(jar, config))
								else
									signOnly(jar, config).toLeft(jar :: Nil)
							case None =>
								if(webstartPack200)
									jarAndPacked(pack200Only(jar))
								else
									Right(jar :: Nil)
						}
					signPackResult.right flatMap { addJars =>
						if(webstartGzip)
						{
							val result = Control.lazyFold(addJars) { jar => gzipJar(jar) }
							result.toLeft(addJars.map(gzipJarPath).toList ::: addJars ::: allJars)
						}
						else
							Right(addJars ::: allJars)
					}
				}
			}
		
			import FileUtilities._
			val jars = (webstartLibraries.get.map(_.asFile) ++ webstartExtraLibraries).filter(ClasspathUtilities.isArchive)
			
			FileTasks.runOption("copy main jar", outputJar from webstartMainJar, log)( copyFile(webstartMainJar, outputJar, log) ) orElse
			thread(copyFilesFlat(jars, webstartLibDirectory, log)) { copiedJars =>
				thread(signAndPack(outputJar :: copiedJars.toList)) { allJars =>
					writeXML(jnlpXML(jarResources(relativize(outputJar), copiedJars.map(relativize))), jnlpFile, log) orElse
					thread(copy(webstartResources.get, webstartOutputDirectory, log)) { copiedResources =>
						val keep = jnlpFile +++ outputJar +++ Path.lazyPathFinder(allJars ++ copiedResources) +++
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
		private def jarResources(mainJar: Path, libraries: Iterable[Path]): Seq[WebstartJarResource] =
			jarResource(true)(mainJar) :: libraries.map(jarResource(false)).toList
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