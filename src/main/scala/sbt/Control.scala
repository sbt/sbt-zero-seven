/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

/** The trap methods execute the provided code in a try block and handle a thrown exception.*/
object Control
{
	def trap[T](errorMessagePrefix: => String, log: Logger)(execute: => Either[String, T]): Either[String, T] =
		try { execute }
		catch { case e => log.trace(e); Left(errorMessagePrefix + e.toString) }
		
	def trapAndFinally[T](errorMessagePrefix: => String, log: Logger)(execute: => Either[String, T])(doFinally: => Unit): Either[String, T] =
		try { execute }
		catch { case e => log.trace(e); Left(errorMessagePrefix + e.toString) }
		finally { doFinally }
		
	def trapUnit(errorMessagePrefix: => String, log: Logger)(execute: => Option[String]): Option[String] =
		try { execute }
		catch { case e => log.trace(e); Some(errorMessagePrefix + e.toString) }
		
	def trapUnitAndFinally(errorMessagePrefix: => String, log: Logger)(execute: => Option[String])(doFinally: => Unit): Option[String] =
		try { execute }
		catch { case e => log.trace(e); Some(errorMessagePrefix + e.toString) }
		finally { doFinally }
		
	def trap(execute: => Unit)
	{
		try { execute }
		catch { case e: Exception => () }
	}
}