package scalaz.example

import scalaz._


object ExampleComp {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    import scalaz.Comp._

    // The composition of two functors is also a functor.
    (List(List(1)).comp.map(2+)) assert_≟ List(List(3))
    List(List(1)).comp.fpair assert_≟ List(List((1, 1)))

    import Digit._
    List("123".toStream).comp.digits assert_≟ List(Stream(some(_1), some(_2), some(_3)))

    // The composition of two applicative functors is also a an applicative functor.
    (some(some(1)).comp |@| some(some(2))) { _ + _ } assert_≟ some(some(3))
  }
}