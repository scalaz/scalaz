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

  val x = 5
  val expectingCorrect = infixNotation(x)
  assert(expectingCorrect === \/-("Correct!"))

  val y = 6
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
     Example #2: Bracket Notation
        Rather than using Infix's Left \/ Right,
        Bracket is \/[Left, Right]. Some IDEs generate Bracket Notation,
        which could lead newcomers to Scalaz to wrongly think that this
        a good way to write these types. Should an IDE generate
        Bracket Notation by default, please replace with Infix Notation.
   */
  def bracketNotation(p: Int): \/[Boolean, String] = p match {
    case 5 => "Correct!".right
    case _ => false.left
  }

  assert(bracketNotation(1) === -\/(false))
  assert(bracketNotation(5) === \/-("Correct!"))
}
