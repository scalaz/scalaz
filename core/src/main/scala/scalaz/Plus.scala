package scalaz

////
/**
 * Universally quantified [[scalaz.Semigroup]].
 */
////
trait Plus[F[_]] extends Functor[F] { self =>
  ////

  def plus[A](a: F[A], b: => F[A]): F[A]

  def semigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def append(f1: F[A], f2: => F[A]): F[A] = plus(f1, f2)
  }

  trait PlusLaw {
    def associative[A](f1: F[A], f2: F[A], f3: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(f1, plus(f2, f3)), plus(plus(f1, f2), f3))
  }
  def plusLaw = new PlusLaw {}
  ////
  val plusSyntax = new scalaz.syntax.PlusSyntax[F] {}
}

object Plus {
  @inline def apply[F[_]](implicit F: Plus[F]): Plus[F] = F

  ////

  ////
}

