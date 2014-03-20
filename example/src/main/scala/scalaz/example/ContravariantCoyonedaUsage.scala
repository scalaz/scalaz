package scalaz.example

import scalaz.{\/, ContravariantCoyoneda => CtCoyo, Order}
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.syntax.monoid._
import scalaz.syntax.order._

object ContravariantCoyonedaUsage extends App {
  // Suppose I have some unstructured data.
  val unstructuredData: List[List[String]] =
    List(List("Zürich", "807", "383,708"),
         List("東京", "1868", "13,185,502"),
         List("Brisbane", "1824-09-13", "2,189,878"),
         List("München", "1158", "1,388,308"),
         List("Boston", "1630-09-07", "636,479"))

  // Or, really, maybe it has some structure.  That's not important.
  // What matters is, I want to sort the data according to various
  // rules.

  def numerically1: Order[String] =
    Order.order((a, b) => parseCommaNum(a) ?|? parseCommaNum(b))

  // Which is a silly way to write this.  Let's try again:

  def numerically2: Order[String] =
    Order orderBy parseCommaNum

  // Which is a shorthand way of writing

  def numerically3: Order[String] =
    Order[String] contramap parseCommaNum

  // All of them have the same behavior: to compare two strings, do
  // `parseCommaNum' on them, and compare the results.  The
  // interesting thing that `numerically3' reveals is that this is
  // just applying the ordinary contravariant functor for `Order'.
  //
  // We'll call `parseCommaNum' a "sort key" function.  Here's that,
  // and the other two we'll be using for this example.

  def parseCommaNum(s: String): Long \/ String =
    """-?[0-9,]+""".r 
}
