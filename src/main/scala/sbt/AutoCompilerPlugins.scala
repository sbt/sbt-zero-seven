/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

trait AutoCompilerPlugins extends BasicScalaProject
{
	abstract override def ivyConfigurations =
	{
		val superConfigurations = super.ivyConfigurations.toList
		val withPlugin = Configurations.CompilerPlugin :: superConfigurations
		val newConfigurations =
			if(superConfigurations.isEmpty)
				 Configurations.Default :: withPlugin
			else
				withPlugin
		log.debug("Auto configurations: " + newConfigurations.toList.mkString(", "))
		Configurations.removeDuplicates(newConfigurations)
	}
	abstract override def compileOptions = compilerPlugins ++ super.compileOptions

	/** A PathFinder that provides the classpath to search for compiler plugins. */
	def pluginClasspath = fullClasspath(Configurations.CompilerPlugin)
	protected def compilerPlugins: List[CompileOption] =
		ClasspathUtilities.compilerPlugins(pluginClasspath.get).map(plugin => new CompileOption("-Xplugin:" + plugin.getAbsolutePath)).toList

	def compilerPlugin(dependency: ModuleID) = dependency % "plugin->default(compile)"
}