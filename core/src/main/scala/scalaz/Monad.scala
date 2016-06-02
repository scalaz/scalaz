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

  /**
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evalated before the loop body.
   * Collects the results into an arbitrary `MonadPlus` value, such as a `List`.
   */
  def whileM[G[_], A](p: F[Boolean], body: => F[A])(implicit G: MonadPlus[G]): F[G[A]] = {
    val f = Need(body)
    ifM(p, bind(f.value)(x => map(whileM(p, f.value))(xs => G.plus(G.point(x), xs))), point(G.empty))
  }

  /**
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evaluated before the loop body.
   * Discards results.
   */
  def whileM_[A](p: F[Boolean], body: => F[A]): F[Unit] = {
    val f = Need(body)
    ifM(p, bind(f.value)(_ => whileM_(p, f.value)), point(()))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Collects results into an
   * arbitrary `MonadPlus` value, such as a `List`.
   */
  def untilM[G[_], A](f: F[A], cond: => F[Boolean])(implicit G: MonadPlus[G]): F[G[A]] = {
    val p = Need(cond)
    bind(f)(x => map(whileM(map(p.value)(!_), f))(xs => G.plus(G.point(x), xs)))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Discards results.
   */
  def untilM_[A](f: F[A], cond: => F[Boolean]): F[Unit] = {
    val p = Need(cond)
    bind(f)(_ => whileM_(map(p.value)(!_), f))
  }

  /**
   * Execute an action repeatedly until its result fails to satisfy the given predicate
   * and return that result, discarding all others.
   */
  def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] =
    bind(f)(y => if (p(y)) iterateWhile(f)(p) else point(y))

  /**
   * Execute an action repeatedly until its result satisfies the given predicate
   * and return that result, discarding all others.
   */
  def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] =
    bind(f)(y => if (p(y)) point(y) else iterateUntil(f)(p))

  /**The product of Monad `F` and `G`, `[x](F[x], G[x]])`, is a Monad */
  def product[G[_]](implicit G0: Monad[G]): Monad[λ[α => (F[α], G[α])]] =
    new ProductMonad[F, G] {
      def F = self
      def G = G0
    }

  trait MonadLaw extends ApplicativeLaw with BindLaw {
    /** Lifted `point` is a no-op. */
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)
    /** Lifted `f` applied to pure `a` is just `f(a)`. */
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
  }
  def monadLaw = new MonadLaw {}
  ////
  val monadSyntax = new scalaz.syntax.MonadSyntax[F] { def F = Monad.this }
}

object Monad {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  ////

  implicit def monadMTMAB[MT[_[_], _], MAB[_, _], A](implicit t: MonadTrans[MT], m: Monad[MAB[A, ?]]): Monad[MT[MAB[A, ?], ?]] = 
    t.apply[MAB[A, ?]]
  
  ////
}
