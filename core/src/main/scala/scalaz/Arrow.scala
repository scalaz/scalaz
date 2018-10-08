package scalaz

////
/**
 * A [[scalaz.Category]] supporting all ordinary functions, as well as
 * combining arrows product-wise.  Every Arrow forms a
 * [[scalaz.Contravariant]] in one type parameter, and a
 * [[scalaz.Applicative]] in the other, just as with ordinary
 * functions.
 */
////
trait Arrow[=>:[_, _]] extends Split[=>:] with Strong[=>:] with Category[=>:] { self =>
  ////

  /** Lift an ordinary function. */
  def arr[A, B](f: A => B): A =>: B

  override def covariantInstance[C]: Applicative[C =>: ?] =
    new Applicative[C =>: ?] with SndCovariant[C] {
      def point[A](a: => A): C =>: A = arr(_ => a)
      def ap[A, B](fa: => (C =>: A))(f: => (C =>: (A => B))): (C =>: B) = <<<(arr((y: (A => B, A)) => y._1(y._2)), combine(f, fa))
    }

  /** Alias for `compose`. */
  final def <<<[A, B, C](fbc: (B =>: C), fab: (A =>: B)): =>:[A, C] =
    compose(fbc, fab)

  /** Flipped `<<<`. */
  def >>>[A, B, C](fab: (A =>: B), fbc: (B =>: C)): (A =>: C) =
    compose(fbc, fab)

  /** Swaps a pair. */
  def swap[X, Y]: ((X, Y) =>: (Y, X)) = arr[(X, Y), (Y, X)] {
    case (x, y) => (y, x)
  }

  /** Pass `C` through untouched. */
  def second[A, B, C](f: (A =>: B)): ((C, A) =>: (C, B)) = {
    >>>(<<<(first[A, B, C](f), swap), swap)
  }

  /** Alias for `split`. */
  final def splitA[A, B, C, D](fab: (A =>: B), fcd: (C =>: D)): ((A, C) =>: (B, D)) =
    split(fab, fcd)

  /** Run `fab` and `fcd` alongside each other.  Sometimes `***`. */
  def split[A, B, C, D](f: A =>: B, g: C =>: D): ((A,  C) =>: (B, D)) =
    >>>(first[A, B, C](f), second[C, D, B](g))

  /** Run two `fab`s alongside each other. */
  def product[A, B](fab: (A =>: B)): ((A, A) =>: (B, B)) =
    splitA(fab, fab)

  /** Run `fab` and `fac` on the same `A`.  Sometimes `&&&`. */
  def combine[A, B, C](fab: (A =>: B), fac: (A =>: C)): (A =>: (B, C)) =
    >>>(arr((a: A) => (a, a)), splitA(fab, fac))

  /** Contramap on `A`. */
  def mapfst[A, B, C](fab: (A =>: B))(f: C => A): (C =>: B) =
    >>>[C, A, B](arr(f), fab)

  /** Functor map on `B`. */
  def mapsnd[A, B, C](fab: (A =>: B))(f: B => C): (A =>: C) =
    <<<[A, B, C](arr(f), fab)

  ////
  val arrowSyntax = new scalaz.syntax.ArrowSyntax[=>:] { def F = Arrow.this }
}

sealed abstract class ArrowInstances0 {

  implicit def arrowDivide[F[_, _], A](implicit A0: Arrow[F], S0: Semigroup[A]): Divide[F[?, A]] = new ArrowDivide[F, A] {
    implicit def A: Arrow[F] = A0
    implicit def S: Semigroup[A] = S0
  }
}

abstract class ArrowInstances extends ArrowInstances0 {

  implicit def arrowApply[F[_, _], A](implicit A0: Arrow[F]): Apply[F[A, ?]] = new ArrowApply[F, A] {
    implicit def A: Arrow[F] = A0
  }

  implicit def arrowDecidable[F[_, _], A](implicit A0: Arrow[F], C0: Choice[F], M0: Monoid[A]): Decidable[F[?, A]] = new ArrowDecidable[F, A] {
    implicit def A: Arrow[F] = A0
    implicit def C: Choice[F] = C0
    implicit def M: Monoid[A] = M0
  }
}

object Arrow extends ArrowInstances {
  @inline def apply[F[_, _]](implicit F: Arrow[F]): Arrow[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Arrow[G]): Arrow[F] =
    new IsomorphismArrow[F, G] {
      override def G: Arrow[G] = E
      override def iso: F <~~> G = D
    }

  ////

  ////
}

trait IsomorphismArrow[F[_, _], G[_, _]] extends Arrow[F] with IsomorphismSplit[F, G] with IsomorphismStrong[F, G] with IsomorphismCategory[F, G]{
  implicit def G: Arrow[G]
  ////

  override def arr[A, B](f: A => B): F[A, B] =
    iso.from(G.arr(f))
  ////
}

private trait ArrowApply[F[_, _], A] extends Apply[F[A, ?]] {
  implicit def A: Arrow[F]

  def map[B, C](fa: F[A, B])(f: B => C): F[A, C] = A.compose(A.arr(f), fa)

  def ap[B,C](fa: => F[A, B])(f: => F[A, B => C]): F[A, C] =
    A.mapsnd(A.combine(f, fa))(t => t._1(t._2))
}

private trait ArrowDecidable[F[_, _], A] extends Decidable[F[?, A]] {
  implicit def A: Arrow[F]
  implicit def C: Choice[F]
  implicit def M: Monoid[A]

  def choose2[Z, A1, A2](a1: =>F[A1, A], a2: =>F[A2, A])(f: Z => (A1 \/ A2)): F[Z, A] =
    A.mapfst(C.choice(a1, a2))(f)
  def divide2[A1, A2, Z](a1: => F[A1, A],a2: => F[A2, A])(f: Z => (A1, A2)): F[Z, A] =
    A.compose(A.arr((t: (A, A)) => M.append(t._1, t._2)), A.compose(A.splitA(a1, a2), A.arr(f)))
  def conquer[A1]: F[A1, A] = A.arr((_: A1) => M.zero)
}

private trait ArrowDivide[F[_, _], A0] extends Divide[F[?, A0]] {
  implicit def A: Arrow[F]
  implicit def S: Semigroup[A0]

  def contramap[A, B](r: F[A, A0])(f: B => A): F[B, A0] = A.mapfst(r)(f)
  def divide2[A1, A2, Z](a1: =>F[A1, A0], a2: =>F[A2, A0])(f: Z => (A1, A2)): F[Z, A0] =
    A.compose(A.arr((t: (A0, A0)) => S.append(t._1, t._2)), A.compose(A.splitA(a1, a2), A.arr(f)))
}
