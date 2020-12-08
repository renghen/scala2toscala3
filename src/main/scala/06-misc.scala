/**
 * TRAIT PARAMETERS
 *
 * Scala 3 introduces trait parameters, which solve a lot of messy initialization order
 * problems in Scala 2.x. 
 */
object trait_parameters:
  trait Console:
    def print(line: String): Unit

  val StandardConsole: Console = println(_)

  /**
   * EXERCISE 1
   * 
   * Remove the field `console`, and instead, introduce a trait parameter.
   */
  trait Logging(val console : Console):
    def log(line: => String): Unit = console.print(line)

  /**
   * EXERCISE 2
   * 
   * Make the following class extend the trait `Logging`, and pass the trait
   * a value for its `Console` parameter.
   */
  class StandardLogger extends Logging(StandardConsole) 

/**
 * EXPLICIT NULLS
 * 
 * When the -Yexplicit-nulls flag is turned on, Scala 3 will treat `Null` as a 
 * subtype of `Any`, and not a supertype of either `AnyRef` or `AnyVal`. Nullable 
 * types are then described with union types.
 */
object explicit_nulls:
  /**
   * EXERCISE 1
   * 
   * Make the following code compile by giving the value a union type that 
   * includes `Null`.
   */
  val stringOrNull: String | Null = null

  def bar = 1

/**
 * CREATOR APPLICATIONS
 * 
 * Scala 3 introduces creator applications, which is a concise way to create instances 
 * of a class even if it does not have an `apply` method in its companion object.
 */
object creator_applications:
  class Logger(printer: String => Unit) {
    var enabled = true 

    def log(s: => String): Unit = if (enabled) printer(s)
  } 

  /**
   * EXERCISE 1
   * 
   * Simplify the construction of this `Logger` by using creator application.
   */
  val logger = Logger(println(_))

/**
 * PROXIES
 * 
 * Scala 3 introduces "proxies", otherwise known as _export clauses_. Export 
 * clauses can automatically create forwarders to the members on another object.
 */
object proxies:
  trait Logger:
    def log(line: String): Unit 

  val ConsoleLogger: Logger = println(_)

  /**
   * EXERCISE 1
   * 
   * Make the following `Console` class extend `Logger`, but rather than 
   * implementing the `log` method directly, export it from the `logger` object.
   */
  class Console extends Logger:
    
    export this.log
    
    def readLine(): String = scala.io.StdIn.readLine()

    def printLine(any: Any): Unit = println(any.toString())
    
/**
 * PARAMETER UNTUPLING
 */
object param_untupling:
  val sum = (x: Int, y: Int) => x + y

  val numbers1 = List(1, 2, 3, 4)
  val numbers2 = List(4, 3, 2, 1)

  /**
   * EXERCISE 1
   * 
   * Map over the "zipped" list of `numbers1` and `numbers2` using the 
   * `sum` function defined above.
   */
  numbers1.zip(numbers2).map(sum(_,_))  

/**
 * CONTEXT FUNCTIONS
 * 
 * Scala 3 introduces context functions, which are functions that depend on some context.
 */
object context_functions:
  trait Program:
    def addOp(op: Op): Unit 
  object Program:
    def make(): Program = 
      var ops = List.empty[Op]
      new Program:
        def addOp(op: Op): Unit = 
          ops = op :: ops


  def addOp(op: Op)(using p: Program) = 
    p.addOp(op)

  enum Op:
    case PushInt(v: Int)
    case Pop
    case Mul 
    case Sub
    case Add

  def op(o: Op): Program ?=> Unit = addOp(o)

  def pushInt(i: Int): Program ?=> Unit = op(Op.PushInt(i))
  val mul: Program ?=> Unit = op(Op.Mul)

  def program[A](f: Program ?=> A): A = 
    given Program = Program.make()
    f 

  program {
    pushInt(12)
    pushInt(23)
    mul
  }

  type Programmable[A] = Program ?=> A
