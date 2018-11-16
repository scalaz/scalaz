package scalaz.example

import scalaz._
import Scalaz._
import scalaz.Strong.uncurry

object StrongUsage extends App {

  // Function1 Strong first
  def len: String => Int = _.length
  (len.first apply ("foo", 14)) assert_=== (3, 14)

  // Function1 Strong uncurry
  val multiplyStrN: String => (Int => String) =
    (str: String) =>
      (times: Int) =>
        str.reverse * times

  (uncurry(multiplyStrN) apply ("evil", 2)) assert_=== "livelive"
}