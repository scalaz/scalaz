package scalaz

import scala.annotation.tailrec
import Maybe.Just

/**
 * A `LeftReducer[C, M]` is a left action of `C` on the semigroup `M`,
 * together with a mapping from `C` to `M` via [[#unit]].
 */
trait LeftReducer[C, M] {
  implicit def semigroup: Semigroup[M]

  def unit(c: C): M

  /**
   * Tack `c` onto `m` from the left.
   * Equivalent to `append(unit(c), m)`, but possibly faster.
   */
  def cons(c: C, m: M): M

  /** Unfold `seed` to the left and reduce. */
  def unfoldlOpt[B](seed: B)(f: B => Maybe[(B, C)]): Maybe[M] = {
    @tailrec
    def rec(seed: B, acc: M): M = f(seed) match {
      case Just((b, c)) => rec(b, cons(c, acc))
      case _ => acc
    }
    f(seed) map { case (b, c) => rec(b, unit(c)) }
  }

  /** Unfold `seed` to the left and reduce. */
  def unfoldl[B](seed: B)(f: B => Maybe[(B, C)])(implicit M: Monoid[M]): M =
    unfoldlOpt(seed)(f) getOrElse M.zero

  trait LeftReducerLaw {
    def consCorrectness(c: C, m: M)(implicit E: Equal[M]): Boolean =
      E.equal(cons(c, m), semigroup.append(unit(c), m))
  }
  def leftReducerLaw = new LeftReducerLaw {}
}
