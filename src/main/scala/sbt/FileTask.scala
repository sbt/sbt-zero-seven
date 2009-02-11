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
	def fileTask(files: ProductsSources)(action: => Option[String]): Task =
		task
		{
			val products = files.products
			existenceCheck(products)(action)
			{
				val sources = files.sources
				if(sources.isEmpty)
				{
					log.debug("Task execution required because no sources exist.")
					action
				}
				else
				{
					val oldestProductModifiedTime = mapLastModified(products).reduceLeft(_ min _)
					val newestSourceModifiedTime = mapLastModified(sources).reduceLeft(_ max _)
					if(oldestProductModifiedTime < newestSourceModifiedTime)
					{
						if(log.atLevel(Level.Debug))
						{
							log.debug("Task execution required because the following sources are newer than at least one product: ")
							logDebugIndented(sources.filter(_.lastModified > oldestProductModifiedTime))
							log.debug(" The following products are older than at least one source: ")
							logDebugIndented(products.filter(_.lastModified < newestSourceModifiedTime))
						}
						action
					}
					else
						None
				}
			}
		}
	def fileTask(products: => Iterable[Path])(action: => Option[String]): Task =
		task { existenceCheck(products)(action)(None) }
	private def existenceCheck(products: Iterable[Path])(action: => Option[String])(ifAllExist: => Option[String]) =
	{
			val nonexisting = products.filter(!_.exists)
			if(nonexisting.isEmpty)
				ifAllExist
			else
			{
				if(log.atLevel(Level.Debug))
				{
					log.debug("Task execution required because at least one product does not exist:")
					logDebugIndented(nonexisting)
				}
				action
			}
	}
	private def logDebugIndented[T](it: Iterable[T]) { it.foreach(x => log.debug("\t" + x)) }
	private def mapLastModified(paths: Iterable[Path]): Iterable[Long] = paths.map(_.lastModified)
}