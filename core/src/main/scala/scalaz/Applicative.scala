package scalaz

////
/**
 * Applicative Functor, described in [[http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects]]
 *
 * Whereas a [[scalaz.Functor]] allows application of a pure function to a value in a context, an Applicative
 * also allows application of a function in a context to a value in a context (`ap`).
 *
 * It follows that a pure function can be applied to arguments in a context. (See `map2`, `map3`, ... )
 *
 * Applicative instances come in a few flavours:
 *  - All [[scalaz.Monad]]s are also `Applicative`
 *  - Any [[scalaz.Monoid]] can be treated as an Applicative (see [[scalaz.Monoid]]#applicative)
 *  - Zipping together corresponding elements of Naperian data structures (those of of a fixed, possibly infinite shape)
 *
 *  @see [[scalaz.Applicative.ApplicativeLaw]]
 */
////
trait Applicative[F[_]] extends Apply[F] with Pointed[F] { self =>
  ////

  // derived functions
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(point(f))

  override def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap2(fa, fb)(point(f))

  // impls of sequence, traverse, etc

  def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(f)(this)

  def sequence[A, G[_]: Traverse](as: G[F[A]]): F[G[A]] =
    traverse(as)(a => a)

  /**The composition of Applicatives `F` and `G`, `[x]F[G[x]]`, is an Applicative */
  def compose[G[_]](implicit G0: Applicative[G]): Applicative[({type λ[α] = F[G[α]]})#λ] = new CompositionApplicative[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Applicatives `F` and `G`, `[x](F[x], G[x]])`, is an Applicative */
  def product[G[_]](implicit G0: Applicative[G]): Applicative[({type λ[α] = (F[α], G[α])})#λ] = new ProductApplicative[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /** An `Applicative` for `F` in which effects happen in the opposite order. */
  def flip: Applicative[F] = new Applicative[F] {
    val F = Applicative.this
    def point[A](a: => A) = F.point(a)
    def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B] =
      F.ap(f)(F.map(fa)(a => (f: A => B) => f(a)))
  }

  trait ApplicativeLaw extends FunctorLaw {
    def identityAp[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(ap(fa)(point((a: A) => a)), fa)

    def composition[A, B, C](fbc: F[B => C], fab: F[A => B], fa: F[A])(implicit FC: Equal[F[C]]) =
      FC.equal(ap(ap(fa)(fab))(fbc), ap(fa)(ap(fab)(ap(fbc)(point((bc: B => C) => (ab: A => B) => bc compose ab)))))

    def homomorphism[A, B](ab: A => B, a: A)(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(point(a))(point(ab)), point(ab(a)))

    def interchange[A, B](f: F[A => B], a: A)(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(point(a))(f), ap(f)(point((f: A => B) => f(a))))
  }
  def applicativeLaw = new ApplicativeLaw {}

  ////
  val applicativeSyntax = new scalaz.syntax.ApplicativeSyntax[F] { def F = Applicative.this }
}

object Applicative {
  @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  ////

  def applicative[F[_]](p: Pointed[F], a: Apply[F]): Applicative[F] = new Applicative[F] {
    def point[A](a: => A): F[A] = p.point(a)
    def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B] = a.ap(fa)(f)
  }

  implicit def monoidApplicative[M:Monoid]: Applicative[({type λ[α] = M})#λ] = Monoid[M].applicative

  ////
}

