package scalaz

import org.scalacheck.Arbitrary
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object StoreTTest extends SpecLite {

  private[this] val storeTestInts = (-10 to 10).toList

  implicit def storeTIntEqual[F[_]: Comonad]: Equal[StoreT[F, Int, Int]] =
    new Equal[StoreT[F, Int, Int]] {
      def equal(a1: StoreT[F, Int, Int], a2: StoreT[F, Int, Int]) =
        (a1.run, a2.run) match {
          case ((tf1, x1), (tf2, x2)) =>
            Equal[Int].equal(x1, x2) && storeTestInts.forall(i =>
              Equal[Int].equal(Comonad[F].copoint(tf1)(i), Comonad[F].copoint(tf2)(i)))
        }
    }

  checkAll(comonad.laws[StoreT[Tuple1, Int, *]])

  checkAll {
    // Not sure why this is needed explicitly
    val am: Arbitrary[Store[Int, Int]]        = implicitly
    val af: Arbitrary[Int => Store[Int, Int]] = implicitly
    val ag: Arbitrary[Store[Int, Int => Int]] = implicitly
    val eq: Equal[Store[Int, Int]]            = implicitly
    monad.laws[Store[Int, *]](implicitly, am, af, ag, eq)
  }

  object instances {
    type A = Int
    def functor[F[_] : Functor] = Functor[StoreT[F, A, *]]
    def cobind[F[_] : Cobind] = Cobind[StoreT[F, A, *]]
    def comonad[F[_] : Comonad] = Comonad[StoreT[F, A, *]]

    // checking absence of ambiguity
    def functor[F[_] : Cobind] = Functor[StoreT[F, A, *]]
    def functor[F[_] : Comonad] = Functor[StoreT[F, A, *]]
    def cobind[F[_] : Comonad] = Cobind[StoreT[F, A, *]]
  }

}
