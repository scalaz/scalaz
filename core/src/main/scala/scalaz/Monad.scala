package scalaz

////
/**
 * Monad, an [[scalaz.Applicative]] that also supports [[scalaz.Bind]],
 * circumscribed by the monad laws.
 *
 * @see [[scalaz.Monad.MonadLaw]]
 */
////
trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  ////

  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => point(f(a)))

  import std.list._

  /** Performs the action `n` times, returning the list of results. */
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    listInstance.sequence(List.fill(n)(fa))(this)

  /** Performs the action `n` times, returning nothing. */
  def replicateM_[A](n: Int, fa: F[A]): F[Unit] =
    listInstance.sequence_(List.fill(n)(fa))(this)

  /** Filter `l` according to a monadic predicate. */
  def filterM[A](l: List[A])(f: A => F[Boolean]): F[List[A]] =
    l match {
      case Nil => point(List())
      case h :: t => bind(f(h))(b => map(filterM(t)(f))(t => if (b) h :: t else t))
    }

  trait MonadLaw extends ApplicativeLaw {
    /** Lifted `point` is a no-op. */
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)
    /** Lifted `f` applied to pure `a` is just `f(a)`. */
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
    /**
     * As with semigroups, monadic effects only change when their
     * order is changed, not when the order in which they're
     * combined changes.
     */
    def associativeBind[A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(bind(bind(fa)(f))(g), bind(fa)((a: A) => bind(f(a))(g)))
  }
  def monadLaw = new MonadLaw {}
  ////
  val monadSyntax = new scalaz.syntax.MonadSyntax[F] { def F = Monad.this }
}

object Monad {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  ////

  ////
}
