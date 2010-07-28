package scalaz.example

import scalaz._
import example.Comp._


object ExampleComp {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val optOptInt = 1.η[Option].η[Option]

//    implicit def OptOptMA[A](ooa: Option[Option[A]]): MA[PartialApplyComp[Option, Option]#Apply, A] = ma[PartialApplyComp[Option, Option]#Apply, A](comp(ooa))
//
//    {
//      (optOptInt ∘ (1 +) value) assert_≟ 2.η[Option].η[Option]
//    }
//    {
//      implicit def OptOptComp[A](ooa: Option[Option[A]]): Comp[Option, Option, A] = comp(ooa)
//      implicit val ap = CompApplicative[Option, Option]
//
//      ((optOptInt |@| optOptInt)(_ + _)(ap, ap) value)   assert_≟ 2.η[Option].η[Option]
//      ((optOptInt |@| optOptInt)(_ + _) value)           assert_≟ 2.η[Option].η[Option]
//    }


    // The composition of two functors is also a functor.
    //    (List(List(1)).comp.map {2 +}) assert_≟ List(List(3))
    //    List(List(1)).comp.fpair assert_≟ List(List((1, 1)))
    //    List("123".toStream).comp.digits assert_≟ List(Stream(some(_1), some(_2), some(_3)))

    // The composition of two applicative functors is also a an applicative functor.
    //    (some(some(1)).comp <*> some(some(2))) {_ + _} assert_≟ some(some(3))
  }
}