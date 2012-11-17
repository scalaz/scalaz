package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scala.util.Random

class DievTest extends Spec {
  val random = new Random()

  "insert order makes no difference" ! check {
    (list: List[Int]) => {
      val shuffledList = random.shuffle(list)
      val dievFromList = list.foldLeft(Diev.empty[Int])(_ + _)
      val dievFromShuffledList = shuffledList.foldLeft(Diev.empty[Int])(_ + _)
      dievFromList must be_===(dievFromShuffledList)
    }
  }

  /*
  checkAll(equal.laws[ListTOpt[Int]])
  checkAll(monoid.laws[ListTOpt[Int]])
  checkAll(plusEmpty.laws[ListTOpt])
  checkAll(monad.laws[ListTOpt])
  checkAll(monadPlus.laws[ListTOpt])

  object instances {
    def semigroup[F[+_]: Monad, A] = Semigroup[ListT[F, A]]
    def monoid[F[+_]: Monad, A] = Monoid[ListT[F, A]]
    def monad[F[+_]: Monad, A] = Monad[({type λ[α]=ListT[F, α]})#λ]
    def functor[F[+_]: Functor, A] = Functor[({type λ[α]=ListT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[+_]: Monad, A] = Functor[({type λ[α]=ListT[F, α]})#λ]  
  }
  */
}
