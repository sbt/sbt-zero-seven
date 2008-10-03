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

trait ConsoleLogger extends BasicLogger
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
		log(successLabelColor, "success", successMessageColor, message)
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
			print(color)
	}
	private def log(labelColor: String, label: String, messageColor: String, message: String) =
	{
		setColor(Console.RESET)
		print('[')
		setColor(labelColor)
		print(label)
		setColor(Console.RESET)
		print("] ")
		setColor(messageColor)
		print(message)
		setColor(Console.RESET)
		println()
	}
}

object Level extends Enumeration with NotNull
{
	val Trace = Value(0, "trace")
	val Debug = Value(1, "debug")
	val Info = Value(2, "info")
	val Warn = Value(3, "warn")
	val Error = Value(4, "error")
	
	def apply(s: String) = elements.find(s == _.toString)
}
