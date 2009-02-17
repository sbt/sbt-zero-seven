/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

import scala.collection.{mutable, Map, Set}

sealed trait ProductsSources extends NotNull
{
	def products: Iterable[Path]
	def sources: Iterable[Path]
}
sealed trait ProductsWrapper extends NotNull
{
	def from(sources: => Iterable[Path]): ProductsSources = from(Path.lazyPathFinder(sources))
	def from(sources: PathFinder): ProductsSources
}
/** Provides methods to define tasks with basic conditional execution based on the sources
* and products of the task. */
trait FileTasks extends Project
{
	implicit def wrapProduct(product: => Path): ProductsWrapper = wrapProducts(product :: Nil)
	implicit def wrapProducts(productsList: => Iterable[Path]): ProductsWrapper =
		new ProductsWrapper
		{
			def from(sourceFinder: PathFinder) =
				new ProductsSources
				{
					def products = productsList
					def sources = sourceFinder.get
				}
		}
	def fileTask(label: String, files: ProductsSources)(action: => Option[String]): Task =
		task { FileTasks.runOption(label, files, log)(action) }
	def fileTask(label: String, products: => Iterable[Path])(action: => Option[String]): Task =
		task { FileTasks.existenceCheck[Option[String]](label, products, log)(action)(None) }
		
	def fileTask(files: ProductsSources)(action: => Option[String]): Task = fileTask("", files)(action)
	def fileTask(products: => Iterable[Path])(action: => Option[String]): Task = fileTask(products)(action)
	
}
object FileTasks
{
	def runOption(label: String, files: ProductsSources, log: Logger)(ifOutofdate: => Option[String]): Option[String] =
	{
		val result = apply[Option[String]](label, files, log)(ifOutofdate)(None)
		if(result.isDefined)
			FileUtilities.clean(files.products, true, log)
		result
	}
	def apply[T](label: String, files: ProductsSources, log: Logger)(ifOutofdate: => T)(ifUptodate: => T): T =
	{
		val products = files.products
		existenceCheck[T](label, products, log)(ifOutofdate)
		{
			val sources = files.sources
			if(sources.isEmpty)
			{
				log.debug("Running " + label + " task because no sources exist.")
				ifOutofdate
			}
			else
			{
				val oldestProductModifiedTime = mapLastModified(products).reduceLeft(_ min _)
				val newestSourceModifiedTime = mapLastModified(sources).reduceLeft(_ max _)
				if(oldestProductModifiedTime < newestSourceModifiedTime)
				{
					if(log.atLevel(Level.Debug))
					{
						log.debug("Running " + label + " task because the following sources are newer than at least one product: ")
						logDebugIndented(sources.filter(_.lastModified > oldestProductModifiedTime), log)
						log.debug(" The following products are older than at least one source: ")
						logDebugIndented(products.filter(_.lastModified < newestSourceModifiedTime), log)
					}
					ifOutofdate
				}
				else
					ifUptodate
			}
		}
}
private def existenceCheck[T](label: String, products: Iterable[Path], log: Logger)(action: => T)(ifAllExist: => T) =
{
		val nonexisting = products.filter(!_.exists)
		if(nonexisting.isEmpty)
			ifAllExist
		else
		{
			if(log.atLevel(Level.Debug))
			{
				log.debug("Running " + label + " task because at least one product does not exist:")
				logDebugIndented(nonexisting, log)
			}
			action
		}
	}
	private def logDebugIndented[T](it: Iterable[T], log: Logger) { it.foreach(x => log.debug("\t" + x)) }
	private def mapLastModified(paths: Iterable[Path]): Iterable[Long] = paths.map(_.lastModified)
}