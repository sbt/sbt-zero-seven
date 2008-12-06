/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.util.regex.Pattern

trait NameFilter extends NotNull
{
	def accept(name: String): Boolean
	def | (filter: NameFilter): NameFilter = new SimpleFilter( name => accept(name) || filter.accept(name) )
	def & (filter: NameFilter): NameFilter = new SimpleFilter( name => accept(name) && filter.accept(name) )
	def - (filter: NameFilter): NameFilter = new SimpleFilter( name => accept(name) && !filter.accept(name) )
	def unary_- : NameFilter = new SimpleFilter( name => !accept(name) )
}
class ExactFilter(matchName: String) extends NameFilter
{
	def accept(name: String) = matchName == name
}
class SimpleFilter(acceptFunction: String => Boolean) extends NameFilter
{
	def accept(name: String) = acceptFunction(name)
}
class PatternFilter(pattern: Pattern) extends NameFilter
{
	def accept(name: String) = pattern.matcher(name).matches
}
object AllPassFilter extends NameFilter
{
	def accept(name: String) = true
}

object GlobFilter
{
	def apply(expression: String): NameFilter =
	{
		if(expression == "*")
			AllPassFilter
		else if(expression.indexOf('*') < 0)
			new ExactFilter(expression)
		else
			new PatternFilter(Pattern.compile(expression.split("\\*").map(Pattern.quote).mkString(".*")))
	}
}