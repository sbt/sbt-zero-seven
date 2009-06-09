/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

import org.scalacheck._

object EnvironmentSpecification extends Properties("Environment")
{ s =>
	private[this] type Env = BasicEnvironment { def x: Property[Int] }
	
	val log = new ConsoleLogger

	specify("Non-optional user property assignment", testAssign _)
	specify("Optional user property assignment", testDefaultAssign _)
	specify("Optional user property default and assignment", testDefault _)
	specify("Optional user property default and then assignment", testDefaultThenAssign _)

	private def testAssign(value: Int) =
	{
		withEnvironment { env =>
			env.x() = value
			env.x.value == value
		}
	}
	private def testDefaultAssign(value: Int, default: Int) =
	{
		withDefaultEnvironment(default) { env =>
			env.x() = value
			env.x.value == value
		}
	}
	private def testDefault(value: Int, default: Int) =
	{
		withDefaultEnvironment(default) { env =>
			env.x.value == default
		}
	}
	private def testDefaultThenAssign(value: Int, default: Int) =
	{
		withDefaultEnvironment(default) { env =>
			env.x.value == default &&
			{
				env.x() = value
				env.x.value == value
			}
		}
	}
	private def defaultEnvironment(default: Int)(backing: Path) = new DefaultEnv(backing) { val x = propertyOptional[Int](default) }
	private def environment(backing: Path) = new DefaultEnv(backing) { val x = property[Int] }
	
	private def withBacking[T](f: Path => T): T = Control.getOrError( FileUtilities.withTemporaryFile(log, "env", "")(file => Right(f(Path.fromFile(file))) ) )
	private def withEnvironment[T](f: Env => T): T = withEnvironmentImpl(environment)(f)
	private def withDefaultEnvironment[T](default: Int)(f: Env => T): T = withEnvironmentImpl(defaultEnvironment(default))(f)
	private def withEnvironmentImpl[T](env: Path => Env)(f: Env => T): T = withBacking(f compose env)

	private class DefaultEnv(val envBackingPath: Path) extends BasicEnvironment { def log = s.log }
}