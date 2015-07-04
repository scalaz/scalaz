package scalaz

import scalaz.syntax.monoid._
import scalaz.syntax.foldable._
import scalaz.std.tuple._
import scalaz.std.vector._

/**
 * The coproduct (or free product) of monoids `M` and `N`.
 * Conceptually this is an alternating list of `M` and `N` values, with
 * the identity as the empty list, and composition as list concatenation that
 * combines adjacent elements when possible.
 */
sealed class :+:[+M, +N](private val rep: Vector[M \/ N]) {
  /** The associative operation of the monoid coproduct */
  def |+|[A >: M : Monoid, B >: N : Monoid](m: A :+: B): A :+: B = {
    @annotation.tailrec
    def go(r1: Vector[A \/ B], r2: Vector[A \/ B]): Vector[A \/ B] =
       (r1, r2) match {
         case (Vector(), es) => es
         case (es, Vector()) => es
         case (v1, v2) => (v1.last, v2.head) match {
           case (-\/(m1), -\/(m2)) => go(v1.init, -\/(m1 |+| m2) +: v2.tail)
           case (\/-(n1), \/-(n2)) => go(v1.init, \/-(n1 |+| n2) +: v2.tail)
           case _ => (v1 ++ v2)
         }
       }
    new :+:(go(rep, m.rep))
  }

  /** Append a value from the left monoid */
  def appendLeft[A >: M : Monoid, B >: N : Monoid](m: A) : A :+: B =
    |+|[A,B](:+:.inL(m))

  /** Append a value from the right monoid */
  def appendRight[A >: M : Monoid, B >: N : Monoid](n: B): A :+: B =
    |+|[A,B](:+:.inR(n))

  /** Prepend a value from the left monoid */
  def prependLeft[A >: M : Monoid, B >: N : Monoid](m: A): A :+: B =
    :+:.inL(m) |+| (this:(A :+: B))

  /** Prepend a value from the right monoid */
  def prependRight[A >: M : Monoid, B >: N : Monoid](n: B): A :+: B =
    :+:.inR(n) |+| (this:(A :+: B))

  /** Project out the value in the left monoid */
  def left[A >: M : Monoid]: A =
    rep.foldLeft(mzero[A]) { (m, e) =>
      m |+| e.fold(a => a, _ => mzero[A])
    }

  /** Project out the value in the right monoid */
  def right[A >: N : Monoid]: A =
    rep.foldLeft(mzero[A]) { (n, e) =>
      n |+| e.fold(_ => mzero[A], a => a)
    }

  /** Project out both monoids individually */
  def both[A >: M : Monoid, B >: N : Monoid]: (A, B) =
    fold(m => (m, mzero[B]), n => (mzero[A], n))

  /** A homomorphism to a monoid `Z` (if `f` and `g` are homomorphisms). */
  def fold[Z:Monoid](f: M => Z, g: N => Z): Z =
    rep.foldMap(_.fold(f, g))

  /**
   * Take a value from the coproduct monoid where each monoid acts on the
   * other, and untangle into a pair of values. Before being folded into the answer
   * an `N` value is combined with the sum of the `M` values to its left via `g` and
   * an `M` value is combined with the sum of the `N` values to its left via `f`.
   * This allows you to add up `N` values while having the opportunity to "track"
   * an evolving `M` value, and vice versa.
   */
  def untangle[A >: M : Monoid, B >: N: Monoid]
    (f: (B, A) => A, g: (A, B) => B): (A, B) =
      rep.foldLeft(mzero[(A, B)]) {
        case ((curm, curn), -\/(m)) =>
          (curm |+| f(curn, m), curn)
        case ((curm, curn), \/-(n)) =>
          (curm, curn |+| g(curm, n))
      }

  /**
   * Like `untangle`, except `M` values are simply combined without regard to the
   * `N` values to the left of it.
   */
  def untangleLeft[A >: M : Monoid, B >: N : Monoid](f: (A, B) => B): (A, B) =
    untangle[A,B]((_, m) => m, f)

  /**
   * Like `untangle`, except `N` values are simply combined without regard to the
   * `N` values to the left of it.
   */
  def untangleRight[A >: M : Monoid, B >: N : Monoid](f: (B, A) => A): (A, B) =
    untangle[A,B](f, (_, n) => n)

}

object :+: {
  import \/._

  def inL[A](a: A): A :+: Nothing = new :+:(Vector(left(a)))
  def inR[A](a: A): Nothing :+: A = new :+:(Vector(right(a)))

  /** The identity of the monoid coproduct */
  def empty[M,N]: M :+: N = new :+:(Vector())

  implicit def monoidCoproductEqual[M: Equal, N: Equal]: Equal[M :+: N] =
    Equal.equalBy(_.rep)

  implicit def instance[M:Monoid,N:Monoid]: Monoid[M :+: N] = new Monoid[M :+: N] {
    val zero = empty
    def append(a: M :+: N, b: => M :+: N) = a |+| b
  }
}
