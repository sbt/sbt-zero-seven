/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import scala.tools.nsc.{io, plugins, symtab, Global, Phase}
import io.{AbstractFile, PlainFile, ZipArchive}
import plugins.{Plugin, PluginComponent}
import symtab.Flags
import scala.collection.mutable.{HashMap, HashSet, Map, Set}

import java.io.File

object Analyzer
{
	val PluginName = "sbt-analyzer"
	val CallbackIDOptionName = "callback:"
}
class Analyzer(val global: Global) extends Plugin
{
	import global._
	import Analyzer._
	
	val name = PluginName
	val description = "A plugin to find all concrete instances of a given class and extract dependency information."
	val components = List[PluginComponent](Component)
	
	private var callbackOption: Option[AnalysisCallback] = None
	
	override def processOptions(options: List[String], error: String => Unit)
	{
		for(option <- options)
		{
			if(option.startsWith(CallbackIDOptionName))
				callbackOption = AnalysisCallback(option.substring(CallbackIDOptionName.length).toInt)
			else
				error("Option not understood: " + option)
		}
		if(callbackOption.isEmpty)
			error("Callback ID not specified.")
	}

	override val optionsHelp: Option[String] = 
	{
		val prefix = "  -P:" + name + ":"
		Some(prefix + CallbackIDOptionName + "<callback-id>            Set the callback id.\n")
	}
	
	private object Component extends PluginComponent
	{
		val global = Analyzer.this.global
		val runsAfter = "jvm"
		val phaseName = Analyzer.this.name
		def newPhase(prev: Phase) = new AnalyzerPhase(prev)
	}

	private class AnalyzerPhase(prev: Phase) extends Phase(prev)
	{
		def name = Analyzer.this.name
		def run
		{
			val callback = callbackOption.get
			val projectPath = callback.basePath
			val projectPathString = Path.basePathString(projectPath).getOrElse({error("Could not determine base path for " + projectPath); ""})
			def relativize(file: File) = Path.relativize(projectPath, projectPathString, file)
			
			val outputDir = new File(global.settings.outdir.value)
			val outputPathOption = relativize(outputDir)
			if(outputPathOption.isEmpty)
				error("Output directory " + outputDir.getAbsolutePath + " must be in the project directory.")
			val outputPath = outputPathOption.get
			
			val superclassNames = callback.superclassNames.map(newTermName)
			val superclassesAll =
				for(name <- superclassNames) yield
				{
					try { Some(global.definitions.getClass(name)) }
					catch { case fe: scala.tools.nsc.FatalError => callback.superclassNotFound(name.toString); None }
				}
			val superclasses = superclassesAll.filter(_.isDefined).map(_.get)
			
			for(unit <- currentRun.units)
			{
				// build dependencies structure
				val sourceFile = unit.source.file.file
				val sourcePathOption = relativize(sourceFile)
				if(sourcePathOption.isEmpty)
					error("Source file " + sourceFile.getAbsolutePath + " must be in the project directory.")
				val sourcePath = sourcePathOption.get
				callback.beginSource(sourcePath)
				for(on <- unit.depends)
				{
					val onSource = on.sourceFile
					if(onSource == null)
					{
						classFile(on) match
						{
							case Some(f) =>
							{
								f match
								{
									case ze: ZipArchive#Entry => callback.jarDependency(new File(ze.getArchive.getName), sourcePath)
									case pf: PlainFile =>
									{
										 // ignore dependencies in the output directory: these are handled by source dependencies
										if(Path.relativize(outputPath, pf.file).isEmpty)
											callback.classDependency(pf.file, sourcePath)
									}
									case _ => ()
								}
							}
							case None => ()
						}
					}
					else
					{
						for(depPath <- relativize(onSource.file))
							callback.sourceDependency(depPath, sourcePath)
					}
				}
				
				// find subclasses
				for(clazz @ ClassDef(mods, n, _, _) <- unit.body)
				{
					val sym = clazz.symbol
					if(mods.isPublic && !mods.isDeferred && !sym.isImplClass && sym.isStatic && !sym.isNestedClass)
					{
						val isModule = sym.isModuleClass
						for(superclass <- superclasses.filter(sym.isSubClass))
							callback.foundSubclass(sourcePath, sym.fullNameString, superclass.fullNameString, isModule)
					}
				}
				
				// build list of generated classes
				for(iclass <- unit.icode)
				{
					val sym = iclass.symbol
					def addGenerated(separatorRequired: Boolean)
					{
						val classPath = pathOfClass(outputPath, sym, separatorRequired)
						if(classPath.asFile.exists)
							callback.generatedClass(sourcePath, classPath)
					}
					if(sym.isModuleClass && !sym.isImplClass)
					{
						if(isTopLevelModule(sym) && sym.linkedClassOfModule == NoSymbol)
							addGenerated(false)
						addGenerated(true)
					}
					else
						addGenerated(false)
				}
				callback.endSource(sourcePath)
			}
		}
	}
	
	private def classFile(sym: Symbol): Option[AbstractFile] =
	{
		import scala.tools.nsc.symtab.Flags
		val name = sym.fullNameString(java.io.File.separatorChar) + (if (sym.hasFlag(Flags.MODULE)) "$" else "")
		val entry = classPath.root.find(name, false)
		if (entry ne null)
			Some(entry.classFile)
		else
			None
	}
	
	private def isTopLevelModule(sym: Symbol): Boolean =
		atPhase (currentRun.picklerPhase.next) {
			sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
		}
	
	private def pathOfClass(outputPath: Path, s: Symbol, separatorRequired: Boolean): Path =
		pathOfClass(outputPath, s, separatorRequired, ".class")
	private def pathOfClass(outputPath: Path, s: Symbol, separatorRequired: Boolean, postfix: String): Path =
	{
		if(s.owner.isPackageClass && s.isPackageClass)
			packagePath(outputPath, s) / postfix
		else
			pathOfClass(outputPath, s.owner.enclClass, true, s.simpleName + (if(separatorRequired) "$" else "") + postfix)
	}
	private def packagePath(outputPath: Path, s: Symbol): Path =
	{
		if(s.isEmptyPackageClass || s.isRoot)
			outputPath
		else
			packagePath(outputPath, s.owner.enclClass) / s.simpleName.toString
	}
}