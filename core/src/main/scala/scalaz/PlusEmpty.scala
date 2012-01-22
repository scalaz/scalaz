package scalaz

////
/**
 * Universally quantified [[scalaz.Monoid]].
 */
////
trait PlusEmpty[F[_]] extends Plus[F] { self =>
  ////
  def empty[A]: F[A]

  // derived functions

  def monoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def append(f1: F[A], f2: => F[A]): F[A] = plus(f1, f2)
    def zero: F[A] = empty[A]
  }

  trait EmptyLaw extends PlusLaw {
    def rightPlusIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(f1, empty[A]), f1)

    def leftPlusIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(empty[A], f1), f1)
  }

  def emptyLaw = new EmptyLaw {}

  ////
  val plusEmptySyntax = new scalaz.syntax.PlusEmptySyntax[F] {}
}

object PlusEmpty {
  @inline def apply[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[F] = F

  ////

  ////
}

