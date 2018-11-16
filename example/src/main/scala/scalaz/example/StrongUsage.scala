package scalaz.example

import scalaz._
import Scalaz._
import scalaz.Strong.uncurry

object StrongUsage extends App {

  // Function1 Strong first
  def len: String => Int = _.length
  (len.first(("foo", 14))) assert_=== ((3, 14))

  // Function1 Strong uncurry
  val multiplyStrN: String => (Int => String) =
    (str: String) =>
      (times: Int) =>
        str.reverse * times

  val evil2: (String, Int) = ("evil", 2)
  val uncurried: ((String,Int)) => String = uncurry(multiplyStrN)
  uncurried(evil2) assert_=== "livelive"
}