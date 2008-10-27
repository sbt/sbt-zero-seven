/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

import java.util.regex.Pattern

trait NameFilter extends NotNull
{
	def accept(name: String): Boolean
	def | (filter: NameFilter): NameFilter = new SimpleFilter( name => accept(name) || filter.accept(name) )
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
class GlobFilter(expression: String) extends PatternFilter(GlobFilter.makePattern(expression))

object GlobFilter
{
	def makePattern(expression: String) = Pattern.compile(expression.split("\\*").map(Pattern.quote).mkString(".*"))
}