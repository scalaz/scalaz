package scalaz

import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.anyVal._

object TracedTTest extends SpecLite {

  private[this] implicit def tracedTNelEqual[A, B](implicit
    A: Arbitrary[A],
    B: Equal[B]
  ): Equal[TracedT[NonEmptyList, A, B]] =
    Equal.equal{ case (TracedT(xs), TracedT(ys)) =>
      (xs.size == ys.size) && Foldable[NonEmptyList].all(xs.zip(ys)){
        case (x, y) =>
          Stream.continually(A.arbitrary.sample).flatten.take(3).forall(
            a => B.equal(x(a), y(a))
          )
      }
    }

  checkAll(comonad.laws[TracedT[NonEmptyList, IList[Boolean], ?]])

  def compilationTestTracedTU: Unit = {
    import scalaz.syntax.either._
    import scalaz.std.function._

    val a: Int \/ (Byte => String) = 1.left[Byte => String]
    TracedT.tracedTU(a)
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[TracedT[F, A, ?]]
    def apply[F[_]: Apply, A] = Apply[TracedT[F, A, ?]]
    def applicative[F[_]: Applicative, A] = Applicative[TracedT[F, A, ?]]
    def distributive[F[_]: Distributive, A] = Distributive[TracedT[F, A, ?]]
    def cobind[F[_]: Cobind, A: Semigroup] = Cobind[TracedT[F, A, ?]]
    def comonoad[F[_]: Comonad, A: Monoid] = Comonad[TracedT[F, A, ?]]
    def comonoadStore[F[_], S, A: Monoid](implicit F: ComonadStore[F, S]) = ComonadStore[TracedT[F, A, ?], S]
    def contravariant[F[_]: Functor, A] = Contravariant[TracedT[F, ?, A]]

    // checking absence of ambiguity
    def functor[F[_]: Apply, A] = Functor[TracedT[F, A, ?]]
    def functor[F[_]: Applicative, A] = Functor[TracedT[F, A, ?]]
    def functor[F[_]: Distributive, A] = Functor[TracedT[F, A, ?]]
    def functor[F[_]: Cobind, A: Monoid] = Functor[TracedT[F, A, ?]]
    def functor[F[_]: Comonad, A: Monoid] = Functor[TracedT[F, A, ?]]
    def functor[F[_], S, A: Monoid](implicit F: ComonadStore[F, S]) = Functor[TracedT[F, A, ?]]
    def apply[F[_]: Applicative, A] = Apply[TracedT[F, A, ?]]
    def cobind[F[_]: Comonad, A: Monoid] = Cobind[TracedT[F, A, ?]]
    def cobind[F[_], S, A: Monoid](implicit F: ComonadStore[F, S]) = Cobind[TracedT[F, A, ?]]
  }

}
