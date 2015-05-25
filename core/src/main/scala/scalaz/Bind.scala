package scalaz

////
/**
 * An [[scalaz.Apply]] functor, where a lifted function can introduce
 * new values _and_ new functor context to be incorporated into the
 * lift context.  The essential new operation of [[scalaz.Monad]]s.
 *
 * @see [[scalaz.Bind.BindLaw]]
 */
////
trait Bind[F[_]] extends Apply[F] { self =>
  ////

  /** Equivalent to `join(map(fa)(f))`. */
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = {
    lazy val fa0 = fa
    bind(f)(map(fa0))
  }

  /** Sequence the inner `F` of `FFA` after the outer `F`, forming a
   * single `F[A]`. */
  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)

  // derived functions

  /**
   * `if` lifted into a binding.  Unlike `lift3((t,c,a)=>if(t)c else
   * a)`, this will only include context from the chosen of `ifTrue`
   * and `ifFalse`, not the other.
   */
  def ifM[B](value: F[Boolean], ifTrue: => F[B], ifFalse: => F[B]): F[B] = {
    val t: () => F[B] = ifTrue _
    val f: () => F[B] = ifFalse _
    bind(value)(if(_) t() else f())
  }

  /**
   * Repeats a monadic action infinitely
   */
  def forever[A, B](fa: F[A]): F[B] = bind(fa)(_ => forever(fa))

  /** Pair `A` with the result of function application. */
  def mproduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)] =
    bind(fa)(a => map(f(a))((a, _)))

  trait BindLaw extends ApplyLaw {
    /**
     * As with semigroups, monadic effects only change when their
     * order is changed, not when the order in which they're
     * combined changes.
     */
    def associativeBind[A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(bind(bind(fa)(f))(g), bind(fa)((a: A) => bind(f(a))(g)))

    /** `ap` is consistent with `bind`. */
    def apLikeDerived[A, B](fa: F[A], f: F[A => B])(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(fa)(f), bind(f)(f => map(fa)(f)))
  }
  def bindLaw = new BindLaw {}

  ////
  val bindSyntax = new scalaz.syntax.BindSyntax[F] { def F = Bind.this }
}

object Bind {
  @inline def apply[F[_]](implicit F: Bind[F]): Bind[F] = F

  ////

  ////
}
