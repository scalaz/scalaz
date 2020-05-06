package scalaz

////
/**
 * Applicative Functor, described in [[https://www.staff.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects]]
 *
 * Whereas a [[scalaz.Functor]] allows application of a pure function to a value in a context, an Applicative
 * also allows application of a function in a context to a value in a context (`ap`).
 *
 * It follows that a pure function can be applied to arguments in a context. (See `apply2`, `apply3`, ... )
 *
 * Applicative instances come in a few flavours:
 *  - All [[scalaz.Monad]]s are also `Applicative`
 *  - Any [[scalaz.Monoid]] can be treated as an Applicative (see [[scalaz.Monoid]]#applicative)
 *  - Zipping together corresponding elements of Naperian data structures (those of of a fixed, possibly infinite shape)
 *
 *  @see [[scalaz.Applicative.ApplicativeLaw]]
 */
////
trait Applicative[F[_]] extends Apply[F] with InvariantApplicative[F] { self =>
  ////
  def point[A](a: => A): F[A]

  // alias for point
  final def pure[A](a: => A): F[A] = point(a)

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

  /**
   * A lawful implementation of this that is isomorphic up to the methods
   * defined on Applicative allowing for optimised parallel implementations that
   * would otherwise violate laws of more specific typeclasses (e.g. Monad).
   */
  def par: Applicative.Par[F] = Tags.Parallel.subst1[Applicative, F](self)

  import std.list._

  override def xproduct0[Z](z: =>Z): F[Z] = point(z)
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z, g: Z => (A1, A2)
  ): F[Z] = apply2(a1, a2)(f)
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = apply3(a1, a2, a3)(f)
  override def xproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = apply4(a1, a2, a3, a4)(f)

  /** Performs the action `n` times, returning the list of results. */
  def replicateM[A](n: Int, fa: F[A]): F[IList[A]] =
    Traverse[IList].sequence(IList.fill(n)(fa))(this)

  /** Performs the action `n` times, returning nothing. */
  def replicateM_[A](n: Int, fa: F[A]): F[Unit] =
    listInstance.sequence_(List.fill(n)(fa))(this)

  /** Filter `map` according to an applicative predicate. **/
  def filterM[A,B](map: A ==>> B)(f: B => F[Boolean])(implicit O:Order[A]):F[A ==>> B] = map.filterM(f)(this, O)

  /** Filter `l` according to an applicative predicate. */
  def filterM[A](l: List[A])(f: A => F[Boolean]): F[List[A]] =
    l match {
      case Nil => point(List())
      case h :: t => ap(filterM(t)(f))(map(f(h))(b => t => if (b) h :: t else t))
    }

  /** Filter `l` according to an applicative predicate. */
  def filterM[A](l: IList[A])(f: A => F[Boolean]): F[IList[A]] = l.filterM(f)(this)

  /**
   * Returns the given argument if `cond` is `false`, otherwise, unit lifted into F.
   */
  def unlessM[A](cond: Boolean)(f: => F[A]): F[Unit] = if (cond) point(()) else void(f)

  /**
   * Returns the given argument if `cond` is `true`, otherwise, unit lifted into F.
   */
  def whenM[A](cond: Boolean)(f: => F[A]): F[Unit] = if (cond) void(f) else point(())

  /**The composition of Applicatives `F` and `G`, `[x]F[G[x]]`, is an Applicative */
  def compose[G[_]](implicit G0: Applicative[G]): Applicative[λ[α => F[G[α]]]] =
    new CompositionApplicative[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Applicatives `F` and `G`, `[x](F[x], G[x]])`, is an Applicative */
  def product[G[_]](implicit G0: Applicative[G]): Applicative[λ[α => (F[α], G[α])]] =
    new ProductApplicative[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /** An `Applicative` for `F` in which effects happen in the opposite order. */
  override def flip: Applicative[F] =
    new Applicative[F] with FlippedApply {
      def point[A](a: => A) = Applicative.this.point(a)
    }

  /** Semigroups can be added within an Applicative */
  def plusA[A](x: => F[A], y: => F[A])(implicit sa: Semigroup[A]): F[A] =
    apply2(x, y)((xa, ya) => sa.append(xa, ya))


  trait ApplicativeLaw extends ApplyLaw {
    /** `point(identity)` is a no-op. */
    def identityAp[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(ap(fa)(point((a: A) => a)), fa)

    /** `point` distributes over function applications. */
    def homomorphism[A, B](ab: A => B, a: A)(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(point(a))(point(ab)), point(ab(a)))

    /** `point` is a left and right identity, F-wise. */
    def interchange[A, B](f: F[A => B], a: A)(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(point(a))(f), ap(f)(point((f: A => B) => f(a))))

    /** `map` is like the one derived from `point` and `ap`. */
    def mapLikeDerived[A, B](f: A => B, fa: F[A])(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(map(fa)(f), ap(fa)(point(f)))
  }
  def applicativeLaw = new ApplicativeLaw {}

  ////
  val applicativeSyntax = new scalaz.syntax.ApplicativeSyntax[F] { def F = Applicative.this }
}

object Applicative {
  @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Applicative[G]): Applicative[F] =
    new IsomorphismApplicative[F, G] {
      override def G: Applicative[G] = E
      override def iso: F <~> G = D
    }

  ////
  type Par[F[_]] = Applicative[λ[α => F[α] @@ Tags.Parallel]]

  ////
}

trait IsomorphismApplicative[F[_], G[_]] extends Applicative[F] with IsomorphismApply[F, G] with IsomorphismInvariantApplicative[F, G]{
  implicit def G: Applicative[G]
  ////

  def point[A](a: => A): F[A] =
    iso.from(G.point(a))

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
    iso.from(G.ap(iso.to(fa))(iso.to(f)))
  override def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    iso.from(G.apply2(iso.to(fa), iso.to(fb))(f))

  override def xproduct0[Z](z: => Z): F[Z] =
    super[Applicative].xproduct0(z)
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    super[Applicative].xproduct1(a1)(f, g)
  override def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    super[Applicative].xproduct2(a1, a2)(f, g)
  override def xproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: (A1, A2, A3) => Z, g: Z => (A1, A2, A3)): F[Z] =
    super[Applicative].xproduct3(a1, a2, a3)(f, g)
  override def xproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    super[Applicative].xproduct4(a1, a2, a3, a4)(f, g)
  ////
}
