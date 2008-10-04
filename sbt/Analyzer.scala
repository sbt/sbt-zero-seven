package sbt

import FileUtilities.printableFilename

import scala.tools.nsc.{plugins, Global, Phase}
import plugins.{Plugin, PluginComponent}
import scala.collection.mutable.{HashMap, HashSet, Map, Set}

import java.io.File

class Analyzer(val global: Global) extends Plugin
{
	import global._
	
	val name = "sbt-analyzer"
	val description = "A plugin to find all concrete instances of a given class"
	val components = List[PluginComponent](Component)
	
	var className: Option[String] = None
	var projectOption: Option[Project] = None
	val ClassOptionName = "class:"
	val ProjectIDOptionName = "project:"
	
	override def processOptions(options: List[String], error: String => Unit)
	{
		for(option <- options)
		{
			if(option.startsWith(ClassOptionName))
				className = Some(option.substring(ClassOptionName.length))
			else if(option.startsWith(ProjectIDOptionName))
				projectOption = Some(Project.project(option.substring(ProjectIDOptionName.length).toInt))
			else
				error("Option not understood: " + option)
		}
		if(className.isEmpty)
			error("Class name not specified.")
		else if(projectOption.isEmpty)
			error("Project ID not specified.")
	}

	override val optionsHelp: Option[String] = 
	{
		val prefix = "  -P:" + name + ":"
		Some(prefix + ClassOptionName +"<className>             Set the name of the class to find to className.\n" +
		     prefix + ProjectIDOptionName + "<project-id>            Set the directory containing (or to contain) the metadata.\n")
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
			val project = projectOption.get
			val analysis = project.analysis
			val projectPath = project.info.projectPath
			val projectPathString = Path.basePathString(projectPath)
			def relativize(file: File) = Path.relativize(projectPath, projectPathString, file)
			
			val outputDir = new File(global.settings.outdir.value)
			val outputPathOption = relativize(outputDir)
			if(outputPathOption.isEmpty)
				error("Output directory " + printableFilename(outputDir) + " must be in the project directory.")
			val outputPath = outputPathOption.get
			
			val propertiesName = newTermName(className.get)
			val propertiesClassOption =
				try { Some(global.definitions.getClass(propertiesName)) }
				catch { case fe: scala.tools.nsc.FatalError => None }
			
			for(unit <- currentRun.units)
			{
				// build dependencies structure
				val sourceFile = unit.source.file.file
				val sourcePathOption = relativize(sourceFile)
				if(sourcePathOption.isEmpty)
					error("Source file " + printableFilename(sourceFile) + " must be in the project directory.")
				val sourcePath = sourcePathOption.get
				analysis.markSource(sourcePath)
				for(on <- unit.depends)
				{
					val onSource = on.sourceFile
					if(onSource != null) // possibly add jar dependencies here
					{
						for(depPath <- relativize(onSource.file))
							analysis.addDependency(depPath, sourcePath)
					}
				}
				analysis.removeSelfDependency(sourcePath)
				
				// build list of tests if ScalaCheck is on the classpath
				for(propertiesClass <- propertiesClassOption;
					clazz @ ClassDef(mods, n, _, _) <- unit.body)
				{
					val sym = clazz.symbol
					if(mods.isPublic && !mods.isDeferred && sym.isModuleClass && !sym.isImplClass && sym.isStatic)
					if(sym.isSubClass(propertiesClass))
						analysis.addTest(sourcePath, sym.fullNameString)
				}
				
				// build list of generated classes
				for(iclass <- unit.icode)
				{
					val sym = iclass.symbol
					if(sym.isModuleClass && !sym.isImplClass)
					{
						if(isTopLevelModule(sym) && sym.linkedClassOfModule == NoSymbol)
						{
							val classPath = pathOfClass(outputPath, sym, false)
							checkPath(unit, classPath)
							analysis.addGeneratedClass(sourcePath, classPath)
						}
						val modulePath = pathOfClass(outputPath, sym, true)
						checkPath(unit, modulePath)
						analysis.addGeneratedClass(sourcePath, modulePath)
					}
					else
					{
						val classPath = pathOfClass(outputPath, sym, false)
						checkPath(unit, classPath)
						analysis.addGeneratedClass(sourcePath, classPath)
					}
				}
			}
		}
	}
	
	private def checkPath(unit: CompilationUnit, path: Path)
	{
		import scala.tools.nsc.util.NoPosition
		if(!path.asFile.exists)
			unit.warning(NoPosition, "Non-existing path in list-class stage: " + path)
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