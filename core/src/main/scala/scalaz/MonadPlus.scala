package scalaz

////
/**
 * @see [[scalaz.Monad]]
 * @see [[scalaz.PlusEmpty]]
 */
////
trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] with MonadPlusParent[F] { self =>
  ////

  /** Remove `f`-failing `A`s in `fa`, by which we mean: in the
    * expression `filter(filter(fa)(f))(g)`, `g` will never be invoked
    * for any `a` where `f(a)` returns false.
    */
  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
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
  final def uniteU[T](value: F[T])(implicit T: Unapply[Foldable, T]): F[T.A] =
    unite(T.leibniz.subst(value))(T.TC)

  /**The product of MonadPlus `F` and `G`, `[x](F[x], G[x]])`, is a MonadPlus */
  def product[G[_]](implicit G0: MonadPlus[G]): MonadPlus[λ[α => (F[α], G[α])]] =
    new ProductMonadPlus[F, G] {
      def F = self
      def G = G0
    }

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

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: MonadPlus[G]): MonadPlus[F] =
    new IsomorphismMonadPlus[F, G] {
      override def G: MonadPlus[G] = E
      override def iso: F <~> G = D
    }

  ////
  /** The Free instruction set for MonadPlus */
  sealed abstract class Ast[A]
  final case class Plus[F[_], A](a: F[A], b: () => F[A]) extends Ast[A]
  final case class Empty[F[_], A]() extends Ast[A]

  /** Extensible Effect */
  def liftF[F[_]](
    implicit I: Ast :<: F
  ): MonadPlus[Free[F, ?]] with BindRec[Free[F, ?]] =
    new MonadPlus[Free[F, ?]] with BindRec[Free[F, ?]] {
      val delegate = Free.freeMonad[F]
      def point[A](a: =>A): Free[F, A] = delegate.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]) = delegate.bind(fa)(f)
      override def map[A, B](fa: Free[F, A])(f: A => B) = delegate.map(fa)(f)
      override def tailrecM[A, B](f: A => Free[F, A \/ B])(a: A) = delegate.tailrecM(f)(a)

      def plus[A](a: Free[F, A], b: =>Free[F, A]): Free[F, A] = Free.liftF(I.inj(Plus[Free[F, ?], A](a, () => b)))
      def empty[A]: Free[F, A] = Free.liftF(I.inj(Empty[F, A]()))
    }

  ////
}
