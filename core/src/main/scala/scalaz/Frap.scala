package scalaz

/**
 * Free applicative functors. Less expressive than free monads, but more
 * flexible to inspect and interpret.
 */
sealed trait Frap[F[_],A] {
  import Frap._

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
  def para[B](pure: A => B, ap: ({type λ[α] = (F[α], Frap[F, α => A])})#λ ~>
                                ({type λ[α] = B})#λ): B =
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
   * def count[F[_],B](p: Frap[F,B]): Int =
   *   p.analyze(new (F ~> ({ type λ[α] = Int })#λ) {
   *     def apply[A](a: F[A]) = 1
   *   })
   * }}}
   */
  def analyze[M:Monoid](f: F ~> ({ type λ[α] = M })#λ): M =
    foldMap[({ type λ[α] = Const[M,α] })#λ](new (F ~> ({ type λ[α] = Const[M,α] })#λ) {
      def apply[X](x: F[X]): Const[M,X] = Const(f(x))
    }).getConst

  /**
   * The natural transformation from `Frap[F,_]` to `Frap[G,_]`
   */
  def hoist[G[_]](f: F ~> G): Frap[G,A] = this match {
    case Pure(a) => Pure(a)
    case x@Ap() => Frap(f(x.v()), x.k() hoist f)
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
  def monadic(implicit F: Functor[F]): Free[F,A] =
    foldMap[({type λ[α] = Free[F,α]})#λ](new (F ~> ({type λ[α] = Free[F,α]})#λ) {
      def apply[B](fb: F[B]) = Free.liftF(fb)
    })

  /** Idiomatic function application */
  def ap[B](f: Frap[F, A => B]): Frap[F,B] = f match {
    case Pure(g) => map(g)
    case x@Ap() => Frap(x.v(), ap(x.k().map(g => (a:A) => (b:x.I) => g(b)(a))))
  }

  /** Append a function to the end of this program */
  def map[B](f: A => B): Frap[F,B] = this match {
    case Pure(a) => Pure(f(a))
    case x@Ap() => Frap(x.v(), x.k().map(f compose _))
  }
}

object Frap {
  implicit def freeInstance[F[_]]: Applicative[({type λ[α] = Frap[F,α]})#λ] =
    new Applicative[({type λ[α] = Frap[F,α]})#λ] {
      def point[A](a: => A) = Frap.point(a)
      def ap[A,B](fa: => Frap[F,A])(ff: => Frap[F, A => B]) = fa ap ff
    }

  /** Return a value in a free applicative functor */
  def point[F[_],A](a: A): Frap[F,A] = Pure(a)

  /** Return a value in a free applicative functor. Alias for `point`. */
  def pure[F[_],A](a: A): Frap[F,A] = point(a)

  /** Lift a value in `F` into the free applicative functor on `F` */
  def lift[F[_],A](x: => F[A]): Frap[F, A] = Frap(x, Pure((a: A) => a))

  private [scalaz] case class Pure[F[_],A](a: A) extends Frap[F,A]
  private abstract case class Ap[F[_],A]() extends Frap[F,A] {
    type I
    val v: () => F[I]
    val k: () => Frap[F, I => A]
  }

  /**
   * Add an effect to the front of a program that produces a continuation for it.
   */
  def apply[F[_],A,B](value: => F[A], function: => Frap[F, A => B]): Frap[F,B] =
    new Ap[F,B] {
      type I = A
      val v = () => value
      val k = () => function
    }
}

