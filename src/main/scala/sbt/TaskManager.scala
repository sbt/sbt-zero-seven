/* sbt -- Simple Build Tool
 * Copyright 2008 David MacIver, Mark Harrah
 */
package sbt

trait TaskManager{
	/** Creates a task that executes the given action when invoked.*/
	def task(action : => Option[String]) = 
		new Task(None, Nil, false, action);
	/** An interactive task is one that is not executed across all dependent projects when
	* it is called directly.  The dependencies of the task are still invoked across all dependent
	* projects, however. */
	def interactiveTask(action: => Option[String]) = new Task(None, Nil, true, action)

	class Task(val description : Option[String], val dependencies : List[Task], val interactive: Boolean,
		action : => Option[String]) extends Dag[Task]
	{
		checkTaskDependencies(dependencies)
		
		/** Creates a new task, identical to this task, except with the additional dependencies specified.*/
		def dependsOn(tasks : Task*) =
		{
			val dependencyList = tasks.toList
			checkTaskDependencies(dependencyList)
			new Task(description, dependencies ::: dependencyList, interactive, action)
		}
		/** Creates a new task, identical to this task, except with the given description.*/
		def describedAs(description : String) = new Task(Some(description), dependencies, interactive, action);
		private[sbt] def invoke = action;

		final def setInteractive = new Task(description, dependencies, true, action)
		final def run = {
			// This is a foldr, but it has the right laziness properties
			def invokeList(tasks : List[Task]) : Option[String] = tasks match {
				case Nil => None;
				case task::more => task.invoke.orElse(invokeList(more)) 
			}
			invokeList(topologicalSort);
		}
		final def runDependenciesOnly =
		{
			// This is a fold, but it has the right laziness properties
			def invokeList(tasks : List[Task]) : Option[String] =
				tasks match
				{
					case Nil=> None
					case ignoreSelf :: Nil => None
					case task :: more => task.invoke.orElse(invokeList(more))
				}
			invokeList(topologicalSort)
		}

		def &&(that : Task) =
			new Task(None, dependencies ::: that.dependencies, interactive || that.interactive, this.invoke.orElse(that.invoke))
	}
	
	private def checkTaskDependencies(dependencyList: List[Task])
	{
		val nullDependencyIndex = dependencyList.findIndexOf(_ == null)
		require(nullDependencyIndex < 0, "Dependency (at index " + nullDependencyIndex + ") is null.  This may be an initialization issue or a circular dependency.")
		val interactiveDependencyIndex = dependencyList.findIndexOf(_.interactive)
		require(interactiveDependencyIndex < 0, "Dependency (at index " + interactiveDependencyIndex + ") is interactive.  Interactive tasks cannot be dependencies.")
	}
}