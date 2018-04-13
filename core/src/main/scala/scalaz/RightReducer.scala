package scalaz

import scala.annotation.tailrec
import Maybe.Just

/**
 * A `RightReducer[C, M]` is a right action of `C` on the semigroup `M`,
 * together with a mapping from `C` to `M` via [[#unit]].
 */
trait RightReducer[C, M] {
  implicit def semigroup: Semigroup[M]

  def unit(c: C): M

  /**
   * Tack `c` onto `m` from the right.
   * Equivalent to `append(m, unit(c))`, but possibly faster.
   */
  def snoc(m: M, c: C): M

  /** Unfold `seed` to the right and reduce. */
  def unfoldrOpt[B](seed: B)(f: B => Maybe[(C, B)]): Maybe[M] = {
    @tailrec
    def rec(acc: M, seed: B): M = f(seed) match {
      case Just((c, b)) => rec(snoc(acc, c), b)
      case _ => acc
    }
    f(seed) map { case (c, b) => rec(unit(c), b) }
  }

  /** Unfold `seed` to the right and reduce. */
  def unfoldr[B](seed: B)(f: B => Maybe[(C, B)])(implicit M: Monoid[M]): M =
    unfoldrOpt(seed)(f) getOrElse M.zero

  trait RightReducerLaw {
    def snocCorrectness(m: M, c: C)(implicit E: Equal[M]): Boolean =
      E.equal(snoc(m, c), semigroup.append(m, unit(c)))
  }
  def rightReducerLaw = new RightReducerLaw {}
}
