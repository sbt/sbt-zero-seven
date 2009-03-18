/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

import java.io.File

import org.apache.ivy.{core, plugins, util, Ivy}
import core.LogOptions
import core.event.EventManager
import core.module.id.ModuleRevisionId
import core.module.descriptor.{Configuration, DefaultDependencyDescriptor, DefaultModuleDescriptor, ModuleDescriptor}
import core.resolve.{ResolveEngine, ResolveOptions}
import core.retrieve.{RetrieveEngine, RetrieveOptions}
import core.sort.SortEngine
import core.settings.IvySettings
import plugins.resolver.{ChainResolver, IBiblioResolver, URLResolver}
import util.{DefaultMessageLogger, Message}

import BootConfiguration._

private[sbt] object UpdateTarget extends Enumeration
{
	val UpdateScala, UpdateSbt = Value
}
import UpdateTarget.{UpdateSbt, UpdateScala}

/** Ensures that the Scala and sbt jars exist for the given versions or else downloads them.*/
private[sbt] final class Update(bootDirectory: File, sbtVersion: String, scalaVersion: String)
{
	//private val logWriter = new FileWriter(new File(bootDirectory, updateLogName))
	Message.setDefaultLogger(SbtIvyLogger)
	def update(target: UpdateTarget.Value)
	{
		/*try { doUpdate(target) }
		catch {*/
		import Configuration.Visibility.PUBLIC
		val moduleID = new DefaultModuleDescriptor(createID(SbtOrg, "boot", "1.0"), "release", null, false)
		moduleID.setLastModified(System.currentTimeMillis)
		moduleID.addConfiguration(new Configuration(DefaultIvyConfiguration, PUBLIC, "", Array(), true, null))
		target match
		{
			case UpdateScala => addDependency(moduleID, ScalaOrg, CompilerModuleName, scalaVersion, "default")
			case UpdateSbt => addDependency(moduleID, SbtOrg, SbtModuleName, sbtVersion, scalaVersion)
		}
		update(moduleID, target)
	}
	private def update(moduleID: DefaultModuleDescriptor,  target: UpdateTarget.Value)
	{
		val eventManager = new EventManager
		val settings = new IvySettings
		addResolvers(settings, scalaVersion, target)
		settings.setDefaultConflictManager(settings.getConflictManager(ConflictManagerName))
		settings.setBaseDir(bootDirectory)
		settings
		resolve(settings, eventManager, moduleID, target)
		retrieve(settings, eventManager, moduleID, target)
	}
	private def createID(organization: String, name: String, revision: String) =
		ModuleRevisionId.newInstance(organization, name, revision)
		
	private def addDependency(moduleID: DefaultModuleDescriptor, organization: String, name: String, revision: String, conf: String)
	{
		val dep = new DefaultDependencyDescriptor(moduleID, createID(organization, name, revision), false, false, true)
		dep.addDependencyConfiguration(DefaultIvyConfiguration, conf)
		moduleID.addDependency(dep)
	}
	
	private def resolve(settings: IvySettings, eventManager: EventManager, module: ModuleDescriptor, target: UpdateTarget.Value)
	{
		val resolveOptions = new ResolveOptions
		resolveOptions.setLog(LogOptions.LOG_DOWNLOAD_ONLY)
		val sortEngine = new SortEngine(settings);
		val resolveEngine = new ResolveEngine(settings, eventManager, sortEngine);
		val resolveReport = resolveEngine.resolve(module, resolveOptions)
		if(resolveReport.hasError)
		{
			println(Set(resolveReport.getAllProblemMessages.toArray: _*).mkString(System.getProperty("line.separator")))
			throw new BootException("Error retrieving required libraries")
		}
	}
	private def retrieve(settings: IvySettings, eventManager: EventManager, module: ModuleDescriptor,  target: UpdateTarget.Value)
	{
		val retrieveOptions = new RetrieveOptions
		val retrieveEngine = new RetrieveEngine(settings, eventManager)
		val pattern =
			target match
			{
				case UpdateSbt => sbtRetrievePattern(sbtVersion) 
				case UpdateScala => scalaRetrievePattern
			}
		retrieveEngine.retrieve(module.getModuleRevisionId, pattern, retrieveOptions);
	}
	private def addResolvers(settings: IvySettings, scalaVersion: String,  target: UpdateTarget.Value)
	{
		val newDefault = new ChainResolver
		newDefault.setName("redefined-public")
		val previousDefault = settings.getDefaultResolver
		if(previousDefault != null) newDefault.add(previousDefault)
		newDefault.add(sbtResolver(scalaVersion))
		newDefault.add(mavenResolver("Scala Tools Releases", "http://scala-tools.org/repo-releases"))
		newDefault.add(mavenResolver("Scala Tools Snapshots", "http://scala-tools.org/repo-snapshots"))
		settings.addResolver(newDefault)
		settings.setDefaultResolver(newDefault.getName)
	}
	private def sbtResolver(scalaVersion: String) =
	{
		val pattern = sbtResolverPattern(scalaVersion)
		val resolver = new URLResolver
		resolver.setName("Sbt Repository")
		resolver.addIvyPattern(pattern)
		resolver.addArtifactPattern(pattern)
		resolver
	}
	private def mavenResolver(name: String, root: String) =
	{
		val resolver = new IBiblioResolver
		resolver.setName(name)
		resolver.setM2compatible(true)
		resolver.setRoot(root)
		resolver
	}
}
/** A custom logger for Ivy to ignore the messages about not finding classes
* intentionally filtered using proguard. */
private final object SbtIvyLogger extends DefaultMessageLogger(Message.MSG_VERBOSE) with NotNull
{
	private val ignorePrefix = "impossible to define"
	override def log(msg: String, level: Int)
	{
		if(level <= getLevel && msg != null && !msg.startsWith(ignorePrefix))
			System.out.println(msg)
	}
	override def rawlog(msg: String, level: Int) { log(msg, level) }
}