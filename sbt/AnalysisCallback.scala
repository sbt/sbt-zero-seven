/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

object AnalysisCallback
{
	private val map = new scala.collection.mutable.HashMap[Int, AnalysisCallback]
	private var nextID: Int = 0
	def register(callback: AnalysisCallback): Int =
	{
		val id = nextID
		nextID += 1
		map(id) = callback
		id
	}
	def apply(id: Int): Option[AnalysisCallback] = map.get(id)
	def unregister(id: Int)
	{
		map -= id
	}
}

trait AnalysisCallback extends NotNull
{
	def superclassNames: Iterable[String]
	def basePath: Path
	
	def superclassNotFound(superclassName: String): Unit
	
	def beginSource(sourcePath: Path): Unit
	def foundSubclass(sourcePath: Path, subclassName: String, superclassName: String, isModule: Boolean): Unit
	def dependency(dependsOnPath: Path, sourcePath: Path): Unit
	def generatedClass(sourcePath: Path, modulePath: Path): Unit
	def endSource(sourcePath: Path): Unit
}
abstract class BasicAnalysisCallback(val basePath: Path, val superclassNames: Iterable[String],
	protected val analysis: ProjectAnalysis) extends AnalysisCallback
{
	def superclassNotFound(superclassName: String) {}
	
	def beginSource(sourcePath: Path)
	{
		analysis.markSource(sourcePath)
	}
	def dependency(dependsOnPath: Path, sourcePath: Path)
	{
		analysis.addDependency(dependsOnPath, sourcePath)
	}
	def generatedClass(sourcePath: Path, modulePath: Path)
	{
		analysis.addGeneratedClass(sourcePath, modulePath)
	}
	def endSource(sourcePath: Path)
	{
		analysis.removeSelfDependency(sourcePath)
	}
}