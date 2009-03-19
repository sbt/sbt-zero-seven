/* sbt -- Simple Build Tool
 * Copyright 2009 Mark Harrah
 */
package sbt

/*
Project does not exist, create new project? [y/N] y
Name: 
Organization [empty]:
Version [1.0]: 
Scala version [2.7.2]: 
sbt version [ 0.3.8 ]: 
*/

/** The constants used by the loader.*/
private object ProjectProperties
{
	/** The properties key for storing the name of the project.*/
	val NameKey = "project.name"
	/** The properties key for storing the organization of the project.*/
	val OrganizationKey = "project.organization"
	/** The properties key for storing the version of the project.*/
	val VersionKey = "project.version"
	/** The properties key for storing the version of Scala used with the project.*/
	val ScalaVersionKey = "scala.version"
	/** The properties key for storing the version of sbt used to build the project.*/
	val SbtVersionKey = "sbt.version"
	/** The properties key to communicate to the main component of sbt that the project
	* should be initialized after being loaded, typically by creating a default directory structure.*/
	val InitializeProjectKey = "project.initialize"
	
	/** The label used when prompting for the name of the user's project.*/
	val NameLabel = "Name"
	/** The label used when prompting for the organization of the user's project.*/
	val OrganizationLabel = "Organization"
	/** The label used when prompting for the version of the user's project.*/
	val VersionLabel = "Version"
	/** The label used when prompting for the version of Scala to use for the user's project.*/
	val ScalaVersionLabel = "Scala version"
	/** The label used when prompting for the version of sbt to use for the user's project.*/
	val SbtVersionLabel = "sbt version"
	
	/** The default organization of the new user project when the user doesn't explicitly specify one when prompted.*/
	val DefaultOrganization = "empty"
	/** The default version of the new user project when the user doesn't explicitly specify a version when prompted.*/
	val DefaultVersion = "1.0"
	/** The default version of sbt when the user doesn't explicitly specify a version when prompted.*/
	val DefaultSbtVersion = "0.3.8"
	/** The default version of Scala when the user doesn't explicitly specify a version when prompted.*/
	val DefaultScalaVersion = "2.7.3"

	import java.io.{File, FileInputStream, FileOutputStream}
	import java.util.Properties
	// (scala version, sbt version)
	def apply(file: File, setInitializeProject: Boolean): (String, String) =
	{
		val properties = new Properties
		if(file.exists)
		{
			val in = new FileInputStream(file)
			try { properties.load(in) } finally { in.close() }
		}
		
		prompt(properties, file.exists)
		if(setInitializeProject)
			properties.setProperty(InitializeProjectKey, true.toString)
		
		file.getParentFile.mkdirs()
		val out = new FileOutputStream(file)
		try { properties.store(out, "Project Properties") } finally { out.close() }
		(properties.getProperty(ScalaVersionKey), properties.getProperty(SbtVersionKey))
	}
	def prompt(fill: Properties, organizationOptional: Boolean)
	{
		val properties =
			(NameKey, NameLabel, None, false) :: 
			(OrganizationKey, OrganizationLabel, Some(DefaultOrganization), organizationOptional) ::
			(VersionKey, VersionLabel, Some(DefaultVersion), false) ::
			(ScalaVersionKey, ScalaVersionLabel, Some(DefaultScalaVersion), false) ::
			(SbtVersionKey, SbtVersionLabel, Some(DefaultSbtVersion), false) ::
			Nil
		for( (key, label, default, optional) <- properties)
		{
			val value = fill.getProperty(key)
			if(value == null && !optional)
				fill.setProperty(key, readLine(label, default))
		}
	}
	private def readLine(label: String, default: Option[String]): String =
	{
		val prompt =
			default match
			{
				case Some(d) => "%s [%s]: ".format(label, d)
				case None => "%s: ".format(label)
			}
		readLine(prompt) orElse default match
		{
			case Some(line) => line
			case None => throw new BootException("Project not loaded: " + label + " not specified.")
		}
	}
	// None if line is null or empty
	private def readLine(prompt: String): Option[String] = process(Console.readLine(prompt))
	private def process(line: String): Option[String] =
		if(line == null)
			None
		else
		{
			val trimmed = line.trim
			if(trimmed.isEmpty)
				None
			else
				Some(trimmed)
		}
}