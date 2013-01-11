package scalaz

////
/**
 * A category supporting all ordinary functions, as well as combining
 * arrows product-wise.  Every Arrow forms a [[scalaz.Contravariant]]
 * in one type parameter, and a [[scalaz.Functor]] in the other, just
 * as with ordinary functions.
 */
////
trait Arrow[=>:[_, _]] extends Category[=>:] { self =>
  ////
  def id[A]: A =>: A

  /** Lift an ordinary function. */
  def arr[A, B](f: A => B): A =>: B

  /** Pass `C` through untouched. */
  def first[A, B, C](f: (A =>: B)): ((A, C) =>: (B, C))

  def covariantInstance[C]: Applicative[({type λ[α] = (C =>: α)})#λ] =
    new Applicative[({type λ[α] = (C =>: α)})#λ] {
      def point[A](a: => A): C =>: A = arr(_ => a)
      def ap[A, B](fa: => (C =>: A))(f: => (C =>: (A => B))): (C =>: B) = <<<(arr((y: (A => B, A)) => y._1(y._2)), combine(f, fa))
      override def map[A, B](fa: (C =>: A))(f: (A) => B): (C =>: B) =
	<<<(arr(f), fa)
    }

  def <<<[A, B, C](fbc: (B =>: C), fab: (A =>: B)): =>:[A, C] =
    compose(fbc, fab)

  def >>>[A, B, C](fab: (A =>: B), fbc: (B =>: C)): (A =>: C) =
    compose(fbc, fab)

  def second[A, B, C](f: (A =>: B)): ((C, A) =>: (C, B)) = {
    def swap[X, Y] = arr[(X, Y), (Y, X)] {
      case (x, y) => (y, x)
    }

    >>>(<<<(first[A, B, C](f), swap), swap)
  }

  // ***
  def splitA[A, B, C, D](fab: (A =>: B), fcd: (C =>: D)): ((A, C) =>: (B, D)) =
      >>>(first[A, B, C](fab), second[C, D, B](fcd))

  def product[A, B](fab: (A =>: B)): ((A, A) =>: (B, B)) =
    splitA(fab, fab)

  // &&&
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

object Arrow {
  @inline def apply[F[_, _]](implicit F: Arrow[F]): Arrow[F] = F

  ////

  ////
}
