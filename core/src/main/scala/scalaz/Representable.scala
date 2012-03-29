package scalaz

/** Representable functors */
abstract class Representable[F[_], X](implicit val F: Functor[F]) {
  def rep[A](f: X => A): F[A]
  def unrep[A](f: F[A]): X => A

  trait RepresentableLaw {
    def repUnrep[A](f: F[A])(implicit E: Equal[F[A]]) =
      E.equal(rep(unrep(f)), f)
    def unrepRep[A](f: X => A, x: X)(implicit E: Equal[A]) =
      E.equal(unrep(rep(f))(x), f(x))
  }
  
  def representableLaw = new RepresentableLaw {}
}

trait RepresentableInstances {
  import scalaz.std.function._

  implicit def readerRepresentable[E]: Representable[({type λ[α] = E => α})#λ, E] =
    new Representable[({type λ[α] = E => α})#λ, E] {
      def rep[A](f: E => A) = f
      def unrep[A](f: E => A) = f
    }

  implicit def curryRepresentable[E]: Representable[({type λ[α] = E => α})#λ, (E, Unit)] =
    new Representable[({type λ[α] = E => α})#λ, (E, Unit)] {
      def rep[A](f: ((E, Unit)) => A): E => A = e => f(e -> ())
      def unrep[A](f: E => A): ((E, Unit)) => A = e => f(e._1)
    }

  implicit val f0Representable: Representable[Function0, Unit] = new Representable[Function0, Unit] {
    def rep[A](f: Unit => A) = () => f(())
    def unrep[A](f: () => A) = u => f()
  }
}

object Representable extends RepresentableInstances

/** Corepresentable functors */
abstract class Corepresentable[F[_], X](implicit F: Contravariant[F]) {
  def corep[A](f: A => X): F[A]
  def uncorep[A](f: F[A]): A => X

  trait CorepresentableLaw {
    def corepUncorep[A](f: F[A])(implicit E: Equal[F[A]]) = 
      E.equal(corep(uncorep(f)), f)
    def uncorepCorep[A](f: A => X, a: A)(implicit E: Equal[X]) =
      E.equal(uncorep(corep(f))(a), f(a))
  }

  def corepresentableLaw = new CorepresentableLaw {}
}

