package scalaz

/** Endomorphisms have special properties among arrows, so are captured in this newtype.
  *
  * Endomorphic[Function1, A] is equivalent to Endo[A]
  */
final case class Endomorphic[=>:[_, _], A](run: A =>: A) {

  final def compose(that: Endomorphic[=>:, A])(implicit F: Compose[=>:]): Endomorphic[=>:, A] =
    Endomorphic[=>:, A](F.compose(run, that.run))

  final def andThen(that: Endomorphic[=>:, A])(implicit F: Compose[=>:]): Endomorphic[=>:, A] =
    that.compose(this)

}

object Endomorphic extends EndomorphicInstances {

  /** Endomorphic Kleisli arrow */
  final def endoKleisli[F[_]: Monad, A](f: A => F[A]): Endomorphic[Kleisli[F, ?, ?], A] =
    Endomorphic[Kleisli[F, ?, ?], A](Kleisli(f))
}

sealed abstract class EndomorphicInstances extends EndomorphicInstances0 {

  implicit def endomorphicMonoid[=>:[_, _], A](implicit G: Category[=>:]): Monoid[Endomorphic[=>:, A]] =
    new Monoid[Endomorphic[=>:, A]] with EndomorphicSemigroup[=>:, A] {
      val F = G
      def zero: Endomorphic[=>:, A] = Endomorphic(G.id)
    }

}

sealed abstract class EndomorphicInstances0 extends EndomorphicInstances1 {

  implicit def kleisliEndoInstance[F[_]: Monad, A]: Monoid[Endomorphic[Kleisli[F, ?, ?], A]] =
    Endomorphic.endomorphicMonoid[Kleisli[F, ?, ?], A]

  implicit def cokleisliEndoInstance[F[_]: Comonad, A]: Monoid[Endomorphic[Cokleisli[F, ?, ?], A]] =
    Endomorphic.endomorphicMonoid[Cokleisli[F, ?, ?], A]

}

sealed abstract class EndomorphicInstances1 extends EndomorphicInstances2 {

  implicit def endomorphicSemigroup[=>:[_, _], A](implicit G: Compose[=>:]): Semigroup[Endomorphic[=>:, A]] =
    new EndomorphicSemigroup[=>:, A] {
      val F = G
    }

}

sealed abstract class EndomorphicInstances2 {

  implicit def kleisliEndoSemigroup[F[_]: Bind, A]: Semigroup[Endomorphic[Kleisli[F, ?, ?], A]] =
    Endomorphic.endomorphicSemigroup[Kleisli[F, ?, ?], A]

  implicit def cokleisliEndoSemigroup[F[_]: Cobind, A]: Semigroup[Endomorphic[Cokleisli[F, ?, ?], A]] =
    Endomorphic.endomorphicSemigroup[Cokleisli[F, ?, ?], A]

}

private trait EndomorphicSemigroup[=>:[_, _], A] extends Semigroup[Endomorphic[=>:, A]] {
  implicit def F: Compose[=>:]
  def append(f1: Endomorphic[=>:, A], f2: => Endomorphic[=>:, A]) = Endomorphic(F.compose(f1.run, f2.run))
}
