/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package sbt

trait Logger extends NotNull
{
	def getLevel: Level.Value
	def setLevel(newLevel: Level.Value)
	def enableTrace(flag: Boolean)
	def traceEnabled: Boolean
	
	def atLevel(level: Level.Value) = level.id >= getLevel.id
	def trace(t: => Throwable): Unit
	def debug(message: => String): Unit = log(Level.Debug, message)
	def info(message: => String): Unit = log(Level.Info, message)
	def warn(message: => String): Unit = log(Level.Warn, message)
	def error(message: => String): Unit = log(Level.Error, message)
	def success(message: => String): Unit = log(Level.Info, message)
	def log(level: Level.Value, message: => String): Unit
	
	def doSynchronized[T](f: => T): T = synchronized { f }
}

/** Implements the level-setting methods of Logger.*/
trait BasicLogger extends Logger
{
	private var traceEnabledVar = false
	private var level: Level.Value = Level.Info
	def getLevel = level
	def setLevel(newLevel: Level.Value)
	{
		level = newLevel
	}
	def enableTrace(flag: Boolean) { traceEnabledVar = flag }
	def traceEnabled = traceEnabledVar
}
/** A logger that can buffer the logging done on it and then flush the buffer to the
* delegate logger provided in the constructor.  Use 'startRecording' to start buffering
* and then 'play' to flush the buffer to the backing logger.  The logging level set at the
* time a message is originally logged is used, not the level at the time 'play' is
* called.
*
* This class assumes that it is the only client of the delegate logger.
*
* This logger is not thread-safe.
* */
final class BufferedLogger(delegate: Logger) extends Logger
{
	private val buffer = new scala.collection.mutable.ListBuffer[LogEvent]
	private var recording = false
	
	private def record(event: => LogEvent)
	{
		if(recording)
			buffer += event
	}
	
	/** Enables buffering. */
	def startRecording()
	{
		recording = true
	}
	/** Flushes the buffer to the delegate logger.  This method executes in the
	* doSynchronized method of the delegate so that the messages are written in order. */
	def play()
	{
		delegate.doSynchronized { buffer.foreach(play) }
	}
	private def play(event: LogEvent)
	{
		event match
		{
			case Success(msg) => delegate.success(msg)
			case Log(level, msg) => delegate.log(level, msg)
			case Trace(t) => delegate.trace(t)
			case SetLevel(level) => delegate.setLevel(level)
			case SetTrace(enabled) => delegate.enableTrace(enabled)
		}
	}
	/** Clears all buffered messages and disables buffering. */
	def clear()
	{
		buffer.clear()
		recording = false
	}
	// these wrap log messages so that they can be replayed later
	private sealed trait LogEvent extends NotNull
	private case class Success(msg: String) extends LogEvent
	private case class Log(level: Level.Value, msg: String) extends LogEvent
	private case class Trace(t: Throwable) extends LogEvent
	private case class SetLevel(newLevel: Level.Value) extends LogEvent
	private case class SetTrace(enabled: Boolean) extends LogEvent
	
	def setLevel(newLevel: Level.Value)
	{
		record(SetLevel(newLevel))
		delegate.setLevel(newLevel)
	}
	def getLevel = delegate.getLevel
	def traceEnabled = delegate.traceEnabled
	def enableTrace(flag: Boolean)
	{
		record(SetTrace(flag))
		delegate.enableTrace(flag)
	}
	
	def trace(t: => Throwable): Unit =
	{
		if(traceEnabled)
		{
			if(recording)
				record(Trace(t))
			else
				delegate.trace(t)
		}
	}
	override def success(message: => String): Unit =
	{
		if(atLevel(Level.Info))
		{
			if(recording)
				record(Success(message))
			else
				delegate.success(message)
		}
	}
	def log(level: Level.Value, message: => String): Unit =
	{
		if(atLevel(level))
		{
			if(recording)
				record(Log(level, message))
			else
				delegate.log(level, message)
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
	def trace(t: => Throwable)
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
			System.out.print(color)
	}
	private def log(labelColor: String, label: String, messageColor: String, message: String) =
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
	
	override def doSynchronized[T](f: => T): T = synchronized { System.out.synchronized { f } }
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

/** Delegates all calls to the Logger provided by 'delegate'.*/
trait DelegatingLogger extends Logger
{
	protected def delegate: Logger
	
	def enableTrace(flag: Boolean) { delegate.enableTrace(flag) }
	def traceEnabled = delegate.traceEnabled
	def getLevel = delegate.getLevel
	def setLevel(newLevel: Level.Value)
	{
		delegate.setLevel(newLevel)
	}
	override def atLevel(level: Level.Value) = delegate.atLevel(level)
	
	def trace(t: => Throwable): Unit = delegate.trace(t)
	override def debug(message: => String): Unit = delegate.debug(message)
	override def info(message: => String): Unit = delegate.info(message)
	override def warn(message: => String): Unit = delegate.warn(message)
	override def error(message: => String): Unit = delegate.error(message)
	override def success(message: => String): Unit = delegate.success(message)
	def log(level: Level.Value, message: => String) = delegate.log(level, message)
	
	override def doSynchronized[T](f: => T): T = delegate.doSynchronized(f)
}