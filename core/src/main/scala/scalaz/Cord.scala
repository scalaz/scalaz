package scalaz

import java.lang.StringBuilder

import scala.annotation.tailrec

/**
 * A `Cord` is a purely functional data structure for efficiently creating a
 * `String` from smaller parts, useful for printing ADTs that must write out
 * their contents into a text format.
 *
 * A `z` interpolator is available for building `String` from literals, using
 * the `Show` typeclass to populate each "hole", and a `cord` interpolator
 * for building `Cord` instances.
 *
 * If a more efficient solution is required to write a large `String` to a
 * network socket or file, consider https://github.com/scalaz/scalaz/issues/1797
 *
 * If you require a general text manipulation data structure, consider using
 * `FingerTree` or creating a custom structure to resemble that used by the
 * popular text editors:
 *
 * - https://ecc-comp.blogspot.co.uk/2015/05/a-brief-glance-at-how-5-text-editors.html
 * - https://pavelfatin.com/typing-with-pleasure/
 */
sealed abstract class Cord {
  /** LEGACY: scalaz <= 7.2 users expect toString to return the underlying String */
  override final def toString: String = shows

  /** Evaluates and stores the String represented by this, returning an equivalent Cord. */
  final def reset: Cord = Cord.Leaf(shows)

  /** Explicitly construct the output String represented by this Cord */
  final def shows: String = this match {
    case Cord.Leaf(str) => str
    case _ =>
      val sb = new StringBuilder
      Cord.unsafeAppendTo(this, sb)
      sb.toString
  }

  // Before adding any methods to this class, please consider if they can be
  // reasonably implemented with future, optimised, representations of Cord. The
  // purpose of this class is to generate Strings as fast as possible, not a
  // general text manipulation structure. For example, a previous version used
  // FingerTree, and many methods were added. But FingerTree was very
  // inefficient at creating Strings yet we could not fix these performance
  // problems because of the need for backcompatibility.

  /**
   * Strict evaluation variant of `Monoid.append` that associates to the right,
   * therefore building more optimal data structures.
   */
  final def ::(o: Cord): Cord = Cord.Branch(o, this)

  /** Strict evaluation variant of `Monoid.append`. */
  final def ++(o: Cord): Cord = Cord.Branch(this, o)
}
object Cord {
  def apply(s: String): Cord = Leaf.apply(s)
  def apply(): Cord = Leaf.Empty

  private[scalaz] final class Leaf private (
    val s: String
  ) extends Cord
  private[scalaz] object Leaf {
    val Empty: Leaf = new Leaf("")
    def apply(s: String): Leaf =
      if (s.isEmpty) Empty
      else new Leaf(s)
    def unapply(l: Leaf): Some[String] = Some(l.s)
  }

  private[scalaz] final class Branch private (
    val leftDepth: Int,
    val left: Cord,
    val right: Cord
  ) extends Cord

  // Limiting the depth of a branch ensures we don't get stack overflows, at the
  // cost of forcing some intermediate strings. We could also have used a DList
  // structure for the left legs, but it would be very inefficient.
  //
  // However, repeated monoidic appends (e.g. foldLeft and fold) produce large
  // LEFT legs that we cannot tail recurse down. Prefer foldRight (or
  // intercalate), or foldLeft with `::` when creating Cord instances for
  // collections.
  private[scalaz] object Branch {
    val max: Int = 100
    def apply(a: Cord, b: Cord): Cord = {
      // avoid constructing a Branch with empty leafs, otherwise Monoid.zero is
      // degenerate over the empty string.
      if (a eq Leaf.Empty) b
      else if (b eq Leaf.Empty) a
      else a match {
        case _: Leaf =>
          new Branch(1, a, b)
        case a: Branch =>
          val branch = new Branch(a.leftDepth + 1, a, b)
          if (a.leftDepth >= max)
            branch.reset
          else
            branch
      }
    }
    def unapply(b: Branch): Some[(Int, Cord, Cord)] = Some((b.leftDepth, b.left, b.right))
  }

  implicit val monoid: Monoid[Cord] = new Monoid[Cord] {
    def zero: Cord                         = Leaf.Empty
    def append(f1: Cord, f2: =>Cord): Cord = Branch(f1, f2)
  }

  implicit val show: Show[Cord] = Show.show(identity)
  implicit val equal: Equal[Cord] = Equal.equal((a, b) => a.shows == b.shows)

  final class CordInterpolator(private val sc: StringContext) extends AnyVal {
    def cord(args: CordInterpolator.Cords*): Cord = {
      import StringContext.treatEscapes
      val strings = IList.fromSeq(sc.parts).map(s => Cord(treatEscapes(s)))
      val cords   = IList.fromSeq(args).map(_.cord)
      strings.interleave(cords).foldRight(Cord())((c, acc) => monoid.append(c, acc))
    }
  }
  object CordInterpolator {
    // the interpolator takes Cords (not Cord) which allows us to restrict the
    // implicit materialisation of Cord (from Show instances) to this usecase.
    final class Cords private (val cord: Cord) extends AnyVal
    object Cords {
      implicit def trivial(c: Cord): Cords   = new Cords(c)
      implicit def mat[A: Show](a: A): Cords = new Cords(Show[A].show(a))
    }
  }

  @tailrec private def unsafeAppendTo(c: Cord, sb: StringBuilder): Unit = c match {
    case Branch(_, a, b) =>
      unsafeAppendTo_(a, sb) // not tail recursive, left legs need to be capped
      unsafeAppendTo(b, sb) // tail recursive, right legs can be arbitrarilly long
    case Leaf(s) =>
      val _ = sb.append(s)
  }
  // breaks out of the tail recursion
  private[this] def unsafeAppendTo_(c: Cord, sb: StringBuilder): Unit = unsafeAppendTo(c, sb)

  type GlorifiedBrick = Cord
}
