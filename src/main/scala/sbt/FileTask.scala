/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

import scala.collection.{mutable, Map, Set}

/** A class that indicates which sources are new or modified and which products are out of date. */
final class FileTaskAnalysis(val sources: Set[Path], val products: Set[Path]) extends NotNull

private object FileTasks
{
	private def wrapThunk[T](f: => Option[String]): (T => Option[String]) = (t: T) => f
}
/** Provides methods to define tasks with basic conditional execution based on the sources
* and products of the task. */
trait FileTasks extends Project
{
	import FileTasks._
	
	/** Creates a task that, when invoked, executes <code>action</code> if any of the paths given
	* by <code>products</code> do not exist.*/
	protected def taskProduceSimple(products: Path*)(action: => Option[String]): Task =
		taskProduce(products: _*)(wrapThunk(action))
	/** Creates a task that, when invoked, executes <code>action</code> if any of the paths given by
	* <code>products</code> do not exist. The <code>action</code> function is passed an object indicating
	* which products did not exist.*/
	protected def taskProduce(products: Path*)(action: FileTaskAnalysis => Option[String]): Task =
	{
		import scala.collection.mutable
		val map = new mutable.HashMap[Path, Iterable[Path]]
		for( product <- products )
			map(product) = Nil
		fileTask(map.readOnly)(action)
	}
	
	/** Creates a task that conditionally executes <code>action</code> when invoked.  The argument
	* <code>productToSource</code> is a list of mappings (<code>Pair</code>s) from a product (a file that
	* the task generates) to the source used to generate it.  A product or a source may occur more than once.
	* A product is marked out of date if it does not exist or if it is older than any sources it depends on.
	* <code>action</code> is executed when the task is invoked only if any products are out of date.
	* Out of date products are deleted before <code>action</code> is called.
	*/
	protected def fileTaskSimple(productToSource: (Path, Path)*)(action: => Option[String]): Task =
		fileTask(productToSource: _*)(wrapThunk(action))
	
	/** Creates a task that conditionally executes <code>action</code> when invoked.  The argument
	* <code>productToSource</code> is a sequence of mappings from a product (a file that
	* the task generates) to the source used to generate it.  A product or a source may occur more than once.
	* A product is marked out of date if it does not exist or if it is older than any sources it depends on.
	
	* When the task is invoked, if any products are out of date, <code>action</code> is called.  It is passed
	* an object representing the out of date products and the sources that were newer or correspond to a product
	* that does not exist.  Out of date products are deleted before <code>action</code> is called.
	*/
	protected def fileTask(productToSource: (Path, Path)*)(action: FileTaskAnalysis => Option[String]): Task =
	{
		import scala.collection.mutable
		val map = new mutable.HashMap[Path, mutable.Set[Path]]
		for( (product, source) <- productToSource )
			map.getOrElseUpdate( product, new mutable.HashSet[Path] ) += source
		fileTask(map.readOnly)(action)
	}
	
	/** Creates a task that conditionally executes <code>action</code> when invoked.  The argument
	* <code>productToSources</code> is a mapping from a product (a file that the task generates)
	* to the source used to generate it.  A source may be a dependency of more than one product.  A product
	* is marked out of date if it does not exist or if it is older than any of the sources it depends on.
	*
	* <code>action</code> is executed when the task is invoked only if any products are out of date.
	* Out of date products are deleted before <code>action</code> is called.
	*/
	protected def fileTaskSimple(productToSources: Map[Path, Iterable[Path]])(action: => Option[String]): Task =
		fileTask(productToSources)(wrapThunk(action))
		
	/** Creates a task that conditionally executes <code>action</code> when invoked.  The argument
	* <code>productToSources</code> maps from a product (a file that the task generates) to the source
	* used to generate it.  A source may be a dependency of more than one product. A product is marked
	* out of date if it does not exist or if it is older than any of the sources it depends on.
	*
	* When the task is invoked, if any products are out of date, <code>action</code> is called.  It is passed
	* an object representing the out of date products and the sources that were newer or correspond to a product
	* that does not exist.  Out of date products are deleted before <code>action</code> is called.
	*/
	protected def fileTask(productToSources: Map[Path, Iterable[Path]])(action: FileTaskAnalysis => Option[String]): Task =
		task
		{
			val newerSources = new scala.collection.mutable.HashSet[Path]
			val outdatedProducts = new scala.collection.mutable.HashSet[Path]
			for( (product, sources) <- productToSources)
			{
				if(product.exists)
				{
					val newerThanProduct = sources.filter(_ newerThan product)
					if(!newerThanProduct.isEmpty)
					{
						newerSources ++= newerThanProduct
						outdatedProducts += product
					}
				}
				else
				{
					newerSources ++= sources
					outdatedProducts += product
				}
			}
			FileUtilities.clean(outdatedProducts, log)
			action(new FileTaskAnalysis(newerSources.readOnly, outdatedProducts.readOnly))
		}
}