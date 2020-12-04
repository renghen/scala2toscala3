/**
 * EXTENSION METHODS
 * 
 * Scala 3 brings first-class support for "extension methods", which allow adding methods to 
 * classes after their definition. Previously, this feature was emulated using implicits.
 */
object ext_methods:
  final case class Email(value: String)

  /**
   * EXERCISE 1
   * 
   * Add an extension method to `Email` to retrieve the username of the email address (the part 
   * of the string before the `@` symbol).
   */
  extension (e: Email) def username: String = e.value.takeWhile(c => c != '@')

  val sherlock = Email("sherlock@holmes.com").username

  /**
   * EXERCISE 2
   * 
   * Add an extension method to `Email` to retrieve the server of the email address (the part of 
   * the string after the `@` symbol).
   */
  // extension
  extension (e: Email) def server: String = e.value.dropWhile(c => c != '@').tail

  /**
   * EXERCISE 3
   * 
   * Add an extension method to `Option[A]` that can zip one option with another `Option[B]`, to 
   * return an `Option[(A, B)]`.
   */
  // extension 

  extension [A,B](oA : Option[A])
    def zip2(oB : Option[B]) : Option[(A,B)] = 
      (oA,oB) match     
        case (Some(a),Some(b)) => Some((a,b))
        case (_,_) => None

  /**
   * A rational number is one in the form n/m, where n and m are integers.
   */
  final case class Rational(numerator: BigInt, denominator: BigInt)
  
  /**
   * EXERCISE 4
   * 
   * Add a collection of extension methods to `Rational`, including `+`, to add two rational 
   * numbers, `*`, to multiply two rational numbers, and `-`, to subtract one rational number 
   * from another rational number.
   */
  
  extension (ratio : Rational)
    def +(that : Rational) : Rational = 
      val numerator = ratio.numerator * that.denominator + that.numerator * ratio.denominator
      Rational(numerator, ratio.denominator * that.denominator )  
    
    def *(that : Rational) : Rational = Rational(ratio.numerator * that.numerator, ratio.denominator * that.denominator)  
  
  @main
  def testw() = 
    val wer = Rational(1,2) + Rational(1,3)
    println(wer)

  /**
   * EXERCISE 5
   * 
   * Convert this implicit syntax class to use extension methods.
   */
  implicit class StringOps(self: String):
    def equalsIgnoreCase(that: String) = self.toLowerCase == that.toLowerCase

  object scope:
    extension (s: String) def isSherlock: Boolean = s.startsWith("Sherlock")

  /**
   * EXERCISE 6
   * 
   * Import the extension method `isSherlock` into the following object so the code will compile.
   */
  object test:
    // "John Watson".isSherlock