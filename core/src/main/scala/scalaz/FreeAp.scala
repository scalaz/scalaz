package scalaz

/**
 * Free applicative functors. Less expressive than free monads, but more
 * flexible to inspect and interpret.
 */
sealed abstract class FreeAp[F[_],A] {
  import FreeAp._

  /**
   * The canonical natural transformation that interprets this free
   * program by giving it the semantics of the applicative functor `G`.
   * Not tail-recursive unless `G` is a free monad.
   */
  def foldMap[G[_]:Applicative](f: F ~> G): G[A] =
    this match {
      case Pure(x) => Applicative[G].pure(x)
      case x@Ap() => Applicative[G].ap(f(x.v()))(x.k() foldMap f)
    }

  /** Provides access to the first instruction of this program, if present */
  def para[B](pure: A => B, ap: λ[α => (F[α], FreeAp[F, α => A])] ~> λ[α => B]): B =
    this match {
      case Pure(x) => pure(x)
      case x@Ap() => ap(x.v() -> x.k())
    }

  /**
   * Performs a monoidal analysis over this free program. Maps the
   * effects in `F` to values in the monoid `M`, discarding the values
   * of those effects.
   * Example:
   *
   * {{{
   * def count[F[_],B](p: FreeAp[F,B]): Int =
   *   p.analyze(new (F ~> λ[α => Int]) {
   *     def apply[A](a: F[A]) = 1
   *   })
   * }}}
   */
  def analyze[M:Monoid](f: F ~> λ[α => M]): M =
    foldMap[Const[M, ?]](
      λ[F ~> Const[M, ?]](x => Const(f(x)))
    ).getConst

  /**
   * The natural transformation from `FreeAp[F,_]` to `FreeAp[G,_]`
   */
  def hoist[G[_]](f: F ~> G): FreeAp[G,A] = this match {
    case Pure(a) => Pure(a)
    case x@Ap() => FreeAp(f(x.v()), x.k() hoist f)
  }

  /**
   * Interprets this free `F` program using the semantics of the
   * `Applicative` instance for `F`.
   */
  def retract(implicit F: Applicative[F]): F[A] = this match {
    case Pure(a) => Applicative[F].pure(a)
    case x@Ap() => Applicative[F].ap(x.v())(x.k().retract)
  }

  /**
   * Embeds this program in the free monad on `F`.
   */
  def monadic: Free[F,A] =
    foldMap[Free[F,?]](λ[F ~> Free[F,?]](Free.liftF(_)))

  /** Idiomatic function application */
  def ap[B](f: FreeAp[F, A => B]): FreeAp[F,B] = f match {
    case Pure(g) => map(g)
    case x@Ap() => FreeAp(x.v(), ap(x.k().map(g => (a:A) => (b:x.I) => g(b)(a))))
  }

  /** Append a function to the end of this program */
  def map[B](f: A => B): FreeAp[F,B] = this match {
    case Pure(a) => Pure(f(a))
    case x@Ap() => FreeAp(x.v(), x.k().map(f compose _))
  }
}

object FreeAp {
  implicit def freeInstance[F[_]]: Applicative[FreeAp[F, ?]] =
    new Applicative[FreeAp[F, ?]] {
      def point[A](a: => A) = FreeAp.point(a)
      def ap[A,B](fa: => FreeAp[F,A])(ff: => FreeAp[F, A => B]) = fa ap ff
    }

  /** Return a value in a free applicative functor */
  def point[F[_],A](a: A): FreeAp[F,A] = Pure(a)

  /** Return a value in a free applicative functor. Alias for `point`. */
  def pure[F[_],A](a: A): FreeAp[F,A] = point(a)

  /** Lift a value in `F` into the free applicative functor on `F` */
  def lift[F[_],A](x: => F[A]): FreeAp[F, A] = FreeAp(x, Pure((a: A) => a))

  /** A version of `lift` that infers the nested type constructor. */
  def liftU[FA](x: => FA)(implicit FA: Unapply[Functor, FA]): FreeAp[FA.M, FA.A] =
    lift(FA(x))

  private [scalaz] case class Pure[F[_],A](a: A) extends FreeAp[F,A]
  private abstract case class Ap[F[_],A]() extends FreeAp[F,A] {
    type I
    val v: () => F[I]
    val k: () => FreeAp[F, I => A]
  }

  /**
   * Add an effect to the front of a program that produces a continuation for it.
   */
  def apply[F[_],A,B](value: => F[A], function: => FreeAp[F, A => B]): FreeAp[F,B] =
    new Ap[F,B] {
      type I = A
      val v = () => value
      val k = () => function
    }
}

