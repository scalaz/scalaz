package scalaz

/** Endomorphisms have special properties among arrows, so are captured in this newtype.
  *
  * Endomorphic[Function1, A] is equivalent to Endo[A]
  */
sealed trait Endomorphic[=>:[_, _], A] {

  def run: A =>: A

  final def compose(that: Endomorphic[=>:, A])(implicit F: Compose[=>:]): Endomorphic[=>:, A] =
    Endomorphic[=>:, A](F.compose(run, that.run))

  final def andThen(that: Endomorphic[=>:, A])(implicit F: Compose[=>:]): Endomorphic[=>:, A] =
    that.compose(this)

}

object Endomorphic extends EndomorphicInstances with EndomorphicFunctions {

  def apply[=>:[_, _], A](ga: A =>: A) = new Endomorphic[=>:, A] {
    val run = ga
  }
}

trait EndomorphicFunctions {

  /** Endomorphic Kleisli arrow */
  final def endoKleisli[F[_]: Monad, A](f: A => F[A]): Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A] =
    Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A](Kleisli(f))
}

sealed abstract class EndomorphicInstances extends EndomorphicInstances0 {

  implicit def endomorphicMonoid[=>:[_, _], A](implicit G: Category[=>:]): Monoid[Endomorphic[=>:, A]] =
    new Monoid[Endomorphic[=>:, A]] with EndomorphicSemigroup[=>:, A] {
      val F = G
      def zero: Endomorphic[=>:, A] = Endomorphic(G.id)
    }

  implicit def kleisliEndoInstance[F[_]: Monad, A]: Monoid[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]] =
    endomorphicMonoid[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]
}

sealed abstract class EndomorphicInstances0 {

  implicit def endomorphicSemigroup[=>:[_, _], A](implicit G: Compose[=>:]): Semigroup[Endomorphic[=>:, A]] =
    new EndomorphicSemigroup[=>:, A] {
      val F = G
    }

}

private[scalaz] trait EndomorphicSemigroup[=>:[_, _], A] extends Semigroup[Endomorphic[=>:, A]] {
  implicit def F: Compose[=>:]
  def append(f1: Endomorphic[=>:, A], f2: => Endomorphic[=>:, A]) = Endomorphic(F.compose(f1.run, f2.run))
}
