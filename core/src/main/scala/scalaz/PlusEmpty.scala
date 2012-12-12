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

  def plusEmptyLaw = new EmptyLaw {}

  ////
  val plusEmptySyntax = new scalaz.syntax.PlusEmptySyntax[F] { def F = PlusEmpty.this }
}

object PlusEmpty {
  @inline def apply[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[F] = F

  ////
  implicit def liftPlusEmpty[M[_], N[_]](implicit M: Monad[M], P: PlusEmpty[N]): PlusEmpty[({ type λ[α] = M[N[α]] })#λ] = new PlusEmpty[({ type λ[α] = M[N[α]] })#λ] {
    def empty[A] = M.point(P.empty[A])
    def plus[A](a: M[N[A]], b: => M[N[A]]): M[N[A]] = {
      M.bind(a) { a0 => M.map(b) { P.plus(a0, _) } }
    }
  }
  ////
}

