/* sbt -- Simple Build Tool
 * Copyright 2008, 2009  Mark Harrah
 */
import sbt._

import java.net.URL

package sbt { // need access to LoaderBase, which is private in package sbt
	object ScriptedLoader
	{
		def apply(paths: Array[URL]): ClassLoader = new ScriptedLoader(paths)
	}
	private class ScriptedLoader(paths: Array[URL]) extends LoaderBase(paths, classOf[ScriptedLoader].getClassLoader)
	{
		def doLoadClass(className: String): Class[_] =
		{
			// Logger needs to be loaded from the version of sbt building the project because we need to pass
			// a Logger from that loader into ScriptedTests.
			// All other sbt classes should be loaded from the project classpath so that we test those classes with 'scripted'
			if(className != "sbt.Logger" && className.startsWith("sbt."))
				findClass(className)
			else
				selfLoadClass(className)
		}
	}
}