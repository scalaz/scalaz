package scalaz.example

import scalaz._
import Scalaz._

object DisjunctionUsage extends App {

  /*
     Example #1: Infix Notation
         Infix notation is commonly used and is easier to read.
         For our first example using disjunctions, this
         is generally an easier notation to follow. The default
         notation to use, Standard Notation, is not as clear
         but will be covered following this example.
   */

  /**
    * Boolean \/ String is another way to represent
    * Either in Scala. It could be viewed as Left \/ Right
    * where Left is considered to be 'incorrect', and Right
    * is considered to be 'correct' (the desired value).
    * The goal is to handle failures elegantly and keep
    * the application running without throwing an exception
    * and terminating.
    *
    * The easiest way to view this: Failure \/ Success
    */
  def infixNotation(p: Int): Boolean \/ String = p match {
    case 5 => \/-("Correct!")
    case _ => -\/(false)
  }

  // Simple val that is assigned the value of 5.
  // We'll use this as a parameter for our above function.
  val x = 5

  // Getting the result, expecting -\/("Correct!")
  val expectingCorrect = infixNotation(x)
  assert(expectingCorrect === \/-("Correct!"))

  // Let's try getting false with another val assigned the
  // value of 6.
  val y = 6

  // Getting the result, expecting \/-(false)
  val expectingFalse = infixNotation(y)
  assert(expectingFalse === -\/(false))

  /*
     A note on the two assertions; one may expect that we should
     have the statement as:
         assert(expectingCorrect === "Correct")
     or
         assert(expectingFalse === false)
     This is because we expect to return *either* -\/ (Left) *or*
     \/- (Right).
  */



  /*
     Example #2: Standard Notation
        Standard Notation is a bit different, rather than using Infix's
        Left \/ Right, Standard is \/[Left, Right]
   */
  def standardNotation(p: Int): \/[Boolean, String] = p match {
    case 5 => \/-("Correct!")
    case _ => -\/(false)
  }

  /*
     Rather than just assigning another val, we can assert
     more elegantly:
  */
  assert(standardNotation(1) === -\/(false))
  assert(standardNotation(5) === \/-("Correct!"))

}
