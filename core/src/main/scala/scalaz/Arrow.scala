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
trait Arrow[=>:[_, _]] extends Split[=>:] with Strong[=>:] with Category[=>:] with ArrowParent[=>:] { self =>
  ////

  /** Lift an ordinary function. */
  def arr[A, B](f: A => B): A =>: B

  override def covariantInstance[C]: Applicative[=>:[C, *]] =
    new Applicative[=>:[C, *]] with SndCovariant[C] {
      def point[A](a: => A): C =>: A = arr(_ => a)
      def ap[A, B](fa: => (C =>: A))(f: => (C =>: (A => B))): (C =>: B) = <<<(arr((y: (A => B, A)) => y._1(y._2)), combine(f, fa))
    }

  /** Alias for `compose`. */
  final def <<<[A, B, C](fbc: (B =>: C), fab: (A =>: B)): =>:[A, C] =
    compose(fbc, fab)

  /** Flipped `<<<`. */
  def >>>[A, B, C](fab: (A =>: B), fbc: (B =>: C)): (A =>: C) =
    compose(fbc, fab)

  /** Pass `C` through untouched. */
  def second[A, B, C](f: (A =>: B)): ((C, A) =>: (C, B)) = {
    def swap[X, Y] = arr[(X, Y), (Y, X)] {
      case (x, y) => (y, x)
    }

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
  val arrowSyntax: scalaz.syntax.ArrowSyntax[=>:] =
    new scalaz.syntax.ArrowSyntax[=>:] { def F = Arrow.this }
}

object Arrow {
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
