package scalaz

import std.anyVal._

/**
 * A `Cord` is a purely functional data structure for efficiently
 * storing and manipulating `String`s that are potentially very long.
 */
sealed trait Cord extends syntax.Ops[FingerTree[Int, String]] {

  import Cord._

  private def rangeError(i: Int) = sys.error("Index out of range: " + i + " >= " + self.measure)

  /** Returns the character at the given position. Throws an error if the index is out of range. */
  def apply(i: Int): Char = {
    val (a, b) = self.split(_ > i)
    b.viewl.headOption.map(_(i - a.measure)).getOrElse(rangeError(i))
  }

  /** Splits this `Cord` in two at the given position. */
  def split(i: Int): (Cord[A], Cord[A]) = {
    val (l, r) = self.split(_ > i)
    val (l1, r1) = r.viewl.headOption.map(_.splitAt(i - l.measure)).getOrElse(rangeError(i))
    (cord(l :+ l1), cord(r1 +: r))
  }

  /** Appends another `Cord` to the end of this one. */
  def ++(xs: Cord): Cord = cord(self <++> xs.self)

  /** Appends a `String` to the end of this `Cord`. */
  def :+(x: => String): Cord = cord(self :+ x)

  /** Prepends a `String` at the beginning of this `Cord`. */
  def +:(x: => String): Cord = cord(x +: self)

  /** Removes the first character of this `Cord`. */
  def tail: Cord = drop(1)

  /** Removes the last character of this `Cord`. */
  def init: Cord = take(self.measure - 1)

  /** Removes the first `n` characters from the front of this `Cord`. */
  def drop(n: Int): Cord = split(n)._2

  /** Returns the first `n` characters at the front of this `Cord`. */
  def take(n: Int): Cord = split(n)._1

  /** Modifies each character in this `Cord` by the given function. */
  def map[B](f: Char => Char): Cord = cord(self map (_ map f))

  def toList: List[Char] = fingerTreeFoldable.foldMap(self, _.toIndexedSeq)
  def toStream: Stream[Char] =
  def toIndexedSeq: IndexedSeq[Char] =

  import FingerTree.fingerTreeFoldable

  /** Transforms each character to a `Cord` according to the given function and concatenates them all into one `Cord`. */
  def flatMap[B](f: Char => Cord): Cord = cord(fingerTreeFoldable.foldLeft(self, FingerTree.empty[Int, String])(
    (ys, x) => ys <++> (x map f).foldRight(Cord())(_ ++ _).self))
}

object Cord {
  private def cord[A](v: FingerTree[Int, String]) = new Cord {
    val self = v
  }

  def apply(as: String*) = fromStrings(as)
  def fromStrings[A](as: Seq[String]) = cord(as.foldLeft(FingerTree.empty[Int, String](sizer))((x, y) => x :+ y))

  implicit def sizer: Reducer[String, Int] = UnitReducer((a: String) => a.length)
}
