/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.io.{File, Writer}

object DotGraph
{
	def apply(analysis: ProjectAnalysis, outputDirectory: Path, log: Logger) =
	{
		val outputDir = outputDirectory.asFile
		
		def generateGraph[Key, Value](fileName: String, graphName: String, graph: Iterable[(Key, scala.collection.Set[Value])],
			keyToString: Key => String, valueToString: Value => String) =
		{
			FileUtilities.write(new File(outputDir, fileName), log)
			{
				(writer: Writer) =>
				{
					def writeLine(line: String) = FileUtilities.writeLine(writer, line)
					writeLine("digraph " + graphName + " {")
					for( (dependsOn, dependants) <- graph; dependant <- dependants)
						writeLine(valueToString(dependant) + " -> " + keyToString(dependsOn))
					writeLine("}")
					None
				}
			}
		}
		FileUtilities.createDirectory(outputDir, log) orElse
		generateGraph(ProjectAnalysis.DependenciesFileName, "dependencies", analysis.allDependencies,
			sourceToString, sourceToString) orElse
		generateGraph(ProjectAnalysis.ExternalDependenciesFileName, "externalDependencies", analysis.allExternalDependencies,
			fileToString, sourceToString)
	}
	private def sourceToString(source: Path) = fileToString(source.asFile)
	private def fileToString(file: File) =
	{
		val rawName = file.getName
		val name =
			if(rawName.endsWith(".scala"))
				rawName.substring(0, rawName.length - ".scala".length)
			else
				rawName
		"\"" + name + "\""
	}
}