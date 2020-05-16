package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._


object TheseTTest extends SpecLite {
  type TheseTList[A, B] = TheseT[List, A, B]
  type TheseTListInt[A] = TheseT[List, Int, A]
  type TheseTOptionInt[A] = TheseT[Option, Int, A]

  checkAll(equal.laws[TheseTListInt[Int]])
  checkAll(semigroup.laws[TheseTListInt[Int]])
  checkAll(monad.laws[TheseTListInt])
  checkAll(traverse.laws[TheseTListInt])
  checkAll(bitraverse.laws[TheseTList])
  checkAll(monadTrans.laws[({type l[a[_], b] = TheseT[a, Int, b]})#l, List])

  object instances {
    def functor[F[_]: Functor, L] = Functor[TheseT[F, L, *]]

    // checking absence of ambiguity
    def functor[F[_]: Traverse, L] = Functor[TheseT[F, L, *]]
    def functor[F[_]: Monad, L: Semigroup] = Functor[TheseT[F, L, *]]
  }
}
