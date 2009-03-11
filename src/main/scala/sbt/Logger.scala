/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

import scala.collection.jcl.WeakHashMap
import scala.collection.mutable.{Buffer, ListBuffer}

sealed trait LogEvent extends NotNull
final class Success(val msg: String) extends LogEvent
final class Log(val level: Level.Value, val msg: String) extends LogEvent
final class Trace(val exception: Throwable) extends LogEvent
final class SetLevel(val newLevel: Level.Value) extends LogEvent
final class SetTrace(val enabled: Boolean) extends LogEvent
final class ControlEvent(val event: ControlEvent.Value, val msg: String) extends LogEvent

object ControlEvent extends Enumeration
{
	val Start, Header, Finish = Value
}

abstract class Logger extends NotNull
{
	def getLevel: Level.Value
	def setLevel(newLevel: Level.Value)
	def enableTrace(flag: Boolean)
	def traceEnabled: Boolean
	
	def atLevel(level: Level.Value) = level.id >= getLevel.id
	def trace(t: => Throwable): Unit
	final def debug(message: => String): Unit = log(Level.Debug, message)
	final def info(message: => String): Unit = log(Level.Info, message)
	final def warn(message: => String): Unit = log(Level.Warn, message)
	final def error(message: => String): Unit = log(Level.Error, message)
	def success(message: => String): Unit
	def log(level: Level.Value, message: => String): Unit
	def control(event: ControlEvent.Value, message: => String): Unit
	
	/** Use this method to ensure calls. */
	def logAll(events: Seq[LogEvent]): Unit
	/** Defined in terms of other methods in Logger and should not be called from them. */
	final def log(event: LogEvent)
	{
		event match
		{
			case s: Success => success(s.msg)
			case l: Log => log(l.level, l.msg)
			case t: Trace => trace(t.exception)
			case setL: SetLevel => setLevel(setL.newLevel)
			case setT: SetTrace => enableTrace(setT.enabled)
			case c: ControlEvent => control(c.event, c.msg)
		}
	}
}

/** Implements the level-setting methods of Logger.*/
abstract class BasicLogger extends Logger
{
	private var traceEnabledVar = false
	private var level: Level.Value = Level.Info
	def getLevel = level
	def setLevel(newLevel: Level.Value) { level = newLevel }
	def enableTrace(flag: Boolean) { traceEnabledVar = flag }
	def traceEnabled = traceEnabledVar
}

final class SynchronizedLogger(delegate: Logger) extends Logger
{
	def getLevel = { synchronized { delegate.getLevel } }
	def setLevel(newLevel: Level.Value) { synchronized { delegate.setLevel(newLevel) } }
	def enableTrace(enabled: Boolean) { synchronized { delegate.enableTrace(enabled) } }
	def traceEnabled: Boolean = { synchronized { delegate.traceEnabled } }
	
	def trace(t: => Throwable) { synchronized { delegate.trace(t) } }
	def log(level: Level.Value, message: => String) { synchronized { delegate.log(level, message) } }
	def success(message: => String) { synchronized { delegate.success(message) } }
	def control(event: ControlEvent.Value, message: => String) { synchronized { delegate.control(event, message) } }
	def logAll(events: Seq[LogEvent]) { synchronized { delegate.logAll(events) } }
}

final class MultiLogger(delegates: List[Logger]) extends BasicLogger
{
	override def setLevel(newLevel: Level.Value)
	{
		super.setLevel(newLevel)
		dispatch(new SetLevel(newLevel))
	}
	override def enableTrace(enabled: Boolean)
	{
		super.enableTrace(enabled)
		dispatch(new SetTrace(enabled))
	}
	def trace(t: => Throwable) { dispatch(new Trace(t)) }
	def log(level: Level.Value, message: => String) { dispatch(new Log(level, message)) }
	def success(message: => String) { dispatch(new Success(message)) }
	def logAll(events: Seq[LogEvent]) { delegates.foreach(_.logAll(events)) }
	def control(event: ControlEvent.Value, message: => String) { delegates.foreach(_.control(event, message)) }
	private def dispatch(event: LogEvent) { delegates.foreach(_.log(event)) }
}

/** Provides a frontend to a logger that provides separate buffers for different calling actors
* Wrap this in SynchronizedLogger. */

/** A logger that can buffer the logging done on it by currently executing actor and
* then can flush the buffer to the delegate logger provided in the constructor.  Use
* 'startRecording' to start buffering and then 'play' from to flush the buffer for the
* current actor to the backing logger.  The logging level set at the
* time a message is originally logged is used, not the level at the time 'play' is
* called.
*
* This class assumes that it is the only client of the delegate logger.
*
* This logger is thread-safe.
* */
final class BufferedLogger(delegate: Logger) extends Logger
{
	import scala.actors.Actor
	private val buffers = new WeakHashMap[Actor, Buffer[LogEvent]]
	private var recording = false
	
	private def buffer = buffers.getOrElseUpdate(Actor.self, new ListBuffer[LogEvent])
	
	/** Enables buffering. */
	def startRecording() { synchronized { recording = true } }
	/** Flushes the buffer to the delegate logger for the current actor.  This method calls logAll on the delegate
	* so that the messages are written consecutively. The buffer is cleared in the process. */
	def play(): Unit = synchronized { delegate.logAll(buffer.readOnly) }
	def playAll(): Unit =
		synchronized
		{
			for(buffer <- buffers.values)
				delegate.logAll(buffer.readOnly)
		}
	/** Clears buffered events for the current actor.  It does not disable buffering. */
	def clear(): Unit = synchronized { buffers.removeKey(Actor.self) }
	/** Clears buffered events for all actors and disables buffering. */
	def clearAll(): Unit =
		synchronized {
			buffers.clear()
			recording = false
		}
	def runAndFlush[T](f: => T): T =
	{
		try { f }
		finally { play();  clear() }
	}
	
	def setLevel(newLevel: Level.Value): Unit =
		synchronized {
			if(recording) buffer += new SetLevel(newLevel)
			delegate.setLevel(newLevel)
		}
	def getLevel = synchronized { delegate.getLevel }
	def traceEnabled = synchronized { delegate.traceEnabled }
	def enableTrace(flag: Boolean): Unit =
		synchronized
		{
			if(recording) buffer += new SetTrace(flag)
			delegate.enableTrace(flag)
		}
	
	def trace(t: => Throwable): Unit =
		synchronized
		{
			if(traceEnabled)
			{
				if(recording) buffer += new Trace(t)
				else delegate.trace(t)
			}
		}
	def success(message: => String): Unit =
		synchronized
		{
			if(atLevel(Level.Info))
			{
				if(recording)
					buffer += new Success(message)
				else
					delegate.success(message)
			}
		}
	def log(level: Level.Value, message: => String): Unit =
		synchronized
		{
			if(atLevel(level))
			{
				if(recording)
					buffer += new Log(level, message)
				else
					delegate.log(level, message)
			}
		}
	def logAll(events: Seq[LogEvent]): Unit =
		synchronized
		{
			if(recording)
				buffer ++= events
			else
				delegate.logAll(events)
		}
	def control(event: ControlEvent.Value, message: => String): Unit =
		synchronized
		{
			if(atLevel(Level.Info))
			{
				if(recording)
					buffer += new ControlEvent(event, message)
				else
					delegate.control(event, message)
			}
		}
}

/** A logger that logs to the console.  On non-windows systems, the level labels are
* colored. 
*
* This logger is not thread-safe.*/
class ConsoleLogger extends BasicLogger
{
	private val os = System.getProperty("os.name")
	private val isWindows = os.toLowerCase.indexOf("windows") >= 0

	def messageColor(level: Level.Value) = Console.RESET
	def labelColor(level: Level.Value) =
		level match
		{
			case Level.Error => Console.RED
			case Level.Warn => Console.YELLOW
			case _ => Console.RESET
		}
	def successLabelColor = Console.GREEN
	def successMessageColor = Console.RESET
	override def success(message: => String)
	{
		if(atLevel(Level.Info))
			log(successLabelColor, Level.SuccessLabel, successMessageColor, message)
	}
	def trace(t: => Throwable): Unit =
		System.out.synchronized
		{
			if(traceEnabled)
				t.printStackTrace
		}
	def log(level: Level.Value, message: => String)
	{
		if(atLevel(level))
			log(labelColor(level), level.toString, messageColor(level), message)
	}
	private def setColor(color: String)
	{
		if(!isWindows)
			System.out.synchronized { System.out.print(color) }
	}
	private def log(labelColor: String, label: String, messageColor: String, message: String): Unit =
		System.out.synchronized
		{
			for(line <- message.split("""\n"""))
			{
				setColor(Console.RESET)
				System.out.print('[')
				setColor(labelColor)
				System.out.print(label)
				setColor(Console.RESET)
				System.out.print("] ")
				setColor(messageColor)
				System.out.print(line)
				setColor(Console.RESET)
				System.out.println()
			}
		}
	
	def logAll(events: Seq[LogEvent]) = System.out.synchronized { events.foreach(log) }
	def control(event: ControlEvent.Value, message: => String)
		{ log(labelColor(Level.Info), Level.Info.toString, Console.BLUE, message) }
}

/** An enumeration defining the levels available for logging.  A level includes all of the levels
* with id larger than its own id.  For example, Warn (id=3) includes Error (id=4).*/
object Level extends Enumeration with NotNull
{
	val Debug = Value(1, "debug")
	val Info = Value(2, "info")
	val Warn = Value(3, "warn")
	val Error = Value(4, "error")
	/** Defines the label to use for success messages.  A success message is logged at the info level but
	* uses this label.  Because the label for levels is defined in this module, the success
	* label is also defined here. */
	val SuccessLabel = "success"
	
	/** Returns the level with the given name wrapped in Some, or None if no level exists for that name. */
	def apply(s: String) = elements.find(s == _.toString)
	/** Same as apply, defined for use in pattern matching. */
	private[sbt] def unapply(s: String) = apply(s)
}