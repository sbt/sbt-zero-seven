/* sbt -- Simple Build Tool
 * Copyright 2008 David MacIver
 */
package sbt

trait TaskManager{
	def task(action : => Option[String]) = 
		new Task(None, Nil, action);

	class Task(val description : Option[String], val dependencies : List[Task],	action : => Option[String]) extends Dag[Task]{
		def dependsOn(tasks : Task*) = new Task(description, dependencies ::: tasks.toList, action);
		def describedAs(description : String) = new Task(Some(description), dependencies, action);
		private def invoke = action;

		final def run = {
			// This is a foldr, but it has the right laziness properties
			def invokeList(tasks : List[Task]) : Option[String] = tasks match {
				case Nil => None;
				case task::more => task.invoke.orElse(invokeList(more)) 
			}
			invokeList(topologicalSort);
		}

		def &&(that : Task) = task { this.invoke.orElse(that.invoke) }
	}
}
