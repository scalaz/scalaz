package scalaz

////
/**
 * @see [[scalaz.Monad]]
 * @see [[scalaz.PlusEmpty]]
 */
////
trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] { self =>
  ////

  /** Remove `f`-failing `A`s in `fa`, by which we mean: in the
    * expression `filter(filter(fa)(f))(g)`, `g` will never be invoked
    * for any `a` where `f(a)` returns false.
    */
  def filter[A](fa: F[A])(f: A => Boolean) =
    bind(fa)(a => if (f(a)) point(a) else empty[A])

  /** Generalized version of Haskell's `catMaybes` */
  def unite[T[_], A](value: F[T[A]])(implicit T: Foldable[T]): F[A] =
    bind(value)((ta) => T.foldMap(ta)(a => point(a))(monoid[A]))

  /** Generalized version of Haskell's `partitionEithers` */
  def separate[G[_, _], A, B](value: F[G[A, B]])(implicit G: Bifoldable[G]): (F[A], F[B]) = {
    val lefts  = bind(value)((aa) => G.leftFoldable.foldMap(aa)(a => point(a))(monoid[A]))
    val rights = bind(value)((bb) => G.rightFoldable.foldMap(bb)(b => point(b))(monoid[B]))
    (lefts, rights)
  }

  /** A version of `unite` that infers the type constructor `T`. */
  final def uniteU[T, A](value: F[T])(implicit T: Unapply[Foldable, T]): F[T.A] =
    unite(T.leibniz.subst(value))(T.TC)

  trait MonadPlusLaw extends EmptyLaw with MonadLaw {
    /** `empty[A]` is a polymorphic value over `A`. */
    def emptyMap[A](f1: A => A)(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(map(empty[A])(f1), empty[A])

    /** `empty` short-circuits its right. */
    def leftZero[A](f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(empty[A])(f), empty[A])
    }
  }
  trait StrongMonadPlusLaw extends MonadPlusLaw {
    /** `empty` short-circuits throughout its `join` tree. */
    def rightZero[A](f: F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(f)(_ => empty[A]), empty[A])
    }
  }
  def monadPlusLaw = new MonadPlusLaw {}
  def strongMonadPlusLaw = new StrongMonadPlusLaw {}
  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] { def F = MonadPlus.this }
}

object MonadPlus {
  @inline def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}
