package scalaz

import org.scalacheck.Arbitrary
import scalaz.std.FunctionTest._
import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._

object SelectTTest extends SpecLite {
  private implicit def selectTEqual[R, M[_], A](implicit E: Equal[(A => M[R]) => M[A]]): Equal[SelectT[R, M, A]] =
    E.contramap(_.run)

  private implicit def selectTArbitrary[R, M[_], A](implicit A: Arbitrary[(A => M[R]) => M[A]]): Arbitrary[SelectT[R, M, A]] =
    Functor[Arbitrary].map(A)(SelectT(_))

  checkAll("Need", monad.laws[SelectT[Int, Need, *]])
  checkAll("Option", monadPlus.strongLaws[SelectT[Int, Option, *]])
  checkAll("List", monadPlus.strongLaws[SelectT[Int, List, *]])
  checkAll("NonEmptyList Monad", monad.laws[SelectT[Int, NonEmptyList, *]])
  checkAll("NonEmptyList Plus", plus.laws[SelectT[Int, NonEmptyList, *]])

  object instances {
    object select {
      def functor[R] = Functor[Select[R, *]]
      def bind[R] = Bind[Select[R, *]]
      def monad[R] = Monad[Select[R, *]]
    }

    def functor[R, M[_]: Functor] = Functor[SelectT[R, M, *]]
    def bind[R, M[_]: Bind] = Bind[SelectT[R, M, *]]
    def monad[R, M[_]: Monad] = Monad[SelectT[R, M, *]]
    def plus[R, M[_]: Plus] = Plus[SelectT[R, M, *]]
    def plusEmpty[R, M[_]: PlusEmpty] = PlusEmpty[SelectT[R, M, *]]
    def monadPlus[R, M[_]: MonadPlus] = MonadPlus[SelectT[R, M, *]]

    // checking absence of ambiguity
    def functor[R, M[_]: Bind] = Functor[SelectT[R, M, *]]
    def functor[R, M[_]: Monad] = Functor[SelectT[R, M, *]]
    def functor[R, M[_]: MonadPlus] = Functor[SelectT[R, M, *]]
    def bind[R, M[_]: Monad] = Bind[SelectT[R, M, *]]
    def bind[R, M[_]: MonadPlus] = Bind[SelectT[R, M, *]]
    def monad[R, M[_]: MonadPlus] = Monad[SelectT[R, M, *]]
    def plus[R, M[_]: PlusEmpty] = Plus[SelectT[R, M, *]]
    def plus[R, M[_]: MonadPlus] = Plus[SelectT[R, M, *]]
    def plusEmpty[R, M[_]: MonadPlus] = PlusEmpty[SelectT[R, M, *]]
  }
}
