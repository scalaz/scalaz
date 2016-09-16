package scalaz

////
/**
 * @see [[scalaz.Monad]]
 * @see [[scalaz.PlusEmpty]]
 */
////
trait MonadPlus[F[_]] extends ApplicativePlus[F] { self =>
  ////
  def monad: Monad[F]

  def applicative: Applicative[F] = monad

  /** Remove `f`-failing `A`s in `fa`, by which we mean: in the
    * expression `filter(filter(fa)(f))(g)`, `g` will never be invoked
    * for any `a` where `f(a)` returns false.
    */
  def filter[A](fa: F[A])(f: A => Boolean) =
    monad.bind(fa)(a => if (f(a)) monad.point(a) else empty[A])

  /** Generalized version of Haskell's `catMaybes` */
  def unite[T[_], A](value: F[T[A]])(implicit T: Foldable[T]): F[A] =
    monad.bind(value)((ta) => T.foldMap(ta)(a => monad.point(a))(monoid[A]))

  /** Generalized version of Haskell's `partitionEithers` */
  def separate[G[_, _], A, B](value: F[G[A, B]])(implicit G: Bifoldable[G]): (F[A], F[B]) = {
    val lefts  = monad.bind(value)((aa) => G.leftFoldable.foldMap(aa)(a => monad.point(a))(monoid[A]))
    val rights = monad.bind(value)((bb) => G.rightFoldable.foldMap(bb)(b => monad.point(b))(monoid[B]))
    (lefts, rights)
  }

  /** A version of `unite` that infers the type constructor `T`. */
  final def uniteU[T](value: F[T])(implicit T: Unapply[Foldable, T]): F[T.A] =
    unite(T.leibniz.subst(value))(T.TC)

  /**The product of MonadPlus `F` and `G`, `[x](F[x], G[x]])`, is a MonadPlus */
  def product[G[_]](implicit G0: MonadPlus[G]): MonadPlus[λ[α => (F[α], G[α])]] =
    new ProductMonadPlus[F, G] {
      def F = self
      def G = G0
    }

  trait MonadPlusLaw extends EmptyLaw {
    /** `empty[A]` is a polymorphic value over `A`. */
    def emptyMap[A](f1: A => A)(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(monad.map(empty[A])(f1), empty[A])

    /** `empty` short-circuits its right. */
    def leftZero[A](f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(monad.bind(empty[A])(f), empty[A])
    }
  }
  trait StrongMonadPlusLaw extends MonadPlusLaw {
    /** `empty` short-circuits throughout its `join` tree. */
    def rightZero[A](f: F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(monad.bind(f)(_ => empty[A]), empty[A])
    }
  }
  def monadPlusLaw = new MonadPlusLaw {}
  def strongMonadPlusLaw = new StrongMonadPlusLaw {}
  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] {
    def F: Monad[F] = MonadPlus.this.monad
    def FE: PlusEmpty[F] = MonadPlus.this
    def FP = MonadPlus.this
  }
}

object MonadPlus {
  @inline def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}
