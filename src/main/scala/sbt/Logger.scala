/* sbt -- Simple Build Tool
 * Copyright 2008 Mark Harrah
 */
package sbt

trait Logger extends NotNull
{
	def getLevel: Level.Value
	def setLevel(newLevel: Level.Value)
	
	def atLevel(level: Level.Value) = level.id >= getLevel.id
	def trace(t: => Throwable): Unit
	def debug(message: => String): Unit = log(Level.Debug, message)
	def info(message: => String): Unit = log(Level.Info, message)
	def warn(message: => String): Unit = log(Level.Warn, message)
	def error(message: => String): Unit = log(Level.Error, message)
	def success(message: => String): Unit = log(Level.Info, message)
	def log(level: Level.Value, message: => String): Unit
}

trait BasicLogger extends Logger
{
	private var level: Level.Value = Level.Info
	def getLevel = level
	def setLevel(newLevel: Level.Value)
	{
		level = newLevel
	}
}
final class BufferedLogger(delegate: Logger) extends Logger
{
	private val buffer = new scala.collection.mutable.ListBuffer[LogEvent]
	private var recording = false
	
	private def record(event: => LogEvent)
	{
		if(recording)
			buffer += event
	}
	
	def startRecording()
	{
		recording = true
	}
	def play()
	{
		System.out.synchronized
		{
			buffer.foreach(play)
		}
	}
	private def play(event: LogEvent)
	{
		event match
		{
			case Success(msg) => delegate.success(msg)
			case Log(level, msg) => delegate.log(level, msg)
			case Trace(t) => delegate.trace(t)
			case SetLevel(level) => delegate.setLevel(level)
		}
	}
	def clear()
	{
		buffer.clear()
		recording = false
	}
	
	sealed trait LogEvent extends NotNull
	case class Success(msg: String) extends LogEvent
	case class Log(level: Level.Value, msg: String) extends LogEvent
	case class Trace(t: Throwable) extends LogEvent
	case class SetLevel(newLevel: Level.Value) extends LogEvent
	
	def setLevel(newLevel: Level.Value)
	{
		record(SetLevel(newLevel))
		delegate.setLevel(newLevel)
	}
	def getLevel = delegate.getLevel
	
	def trace(t: => Throwable): Unit =
	{
		if(atLevel(Level.Trace))
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
		if(atLevel(Level.Trace))
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
}

object Level extends Enumeration with NotNull
{
	val Trace = Value(0, "trace")
	val Debug = Value(1, "debug")
	val Info = Value(2, "info")
	val Warn = Value(3, "warn")
	val Error = Value(4, "error")
	val SuccessLabel = "success"
	
	def apply(s: String) = elements.find(s == _.toString)
}

trait DelegatingLogger extends Logger
{
	protected def delegate: Logger
	
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
}
