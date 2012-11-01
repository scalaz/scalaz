package scalaz

import collection.immutable.IndexedSeq

import std.anyVal._
import std.string._
import std.indexedSeq.indexedSeqMonoid

/**
 * A `Cord` is a purely functional data structure for efficiently
 * storing and manipulating `String`s that are potentially very long.
 * Very similar to `Rope[Char]`, but with better constant factors and a
 * simpler interface since it's specialized for `String`s.
 */
sealed trait Cord extends syntax.Ops[FingerTree[Int, String]] {

  import Cord.{stringToCord => _, _}

  private def rangeError(i: Int) = sys.error("Index out of range: " + i + " >= " + self.measure)

  /**
    * Returns the character at the given position. Throws an error if the index is out of range.
    * Time complexity: O(log N)
    */
  def apply(i: Int): Char = {
    val (a, b) = self.split(_ > i)
    b.viewl.headOption.map(_(i - a.measure)).getOrElse(rangeError(i))
  }

  /**
   * Splits this `Cord` in two at the given position.
   * Time complexity: O(log N)
   */
  def split(i: Int): (Cord, Cord) = {
    val (l, r) = self.split(_ > i)
    val (l1, r1) = r.viewl.headOption.map(_.splitAt(i - l.measure)).getOrElse(rangeError(i))
    (cord(l :+ l1), cord(r1 +: r))
  }

  /**
   * Returns the number of characters in this `Cord`.
   * Time complexity: O(1)
   */
  def length: Int = self.measure

  /**
   * Returns the number of characters in this `Cord`.
   * Time complexity: O(1)
   */
  def size: Int = self.measure

  /**
   * Appends another `Cord` to the end of this one.
   * Time complexity: O(log (min N M)) where M and N are the lengths of the two `Cord`s.
   */
  def ++(xs: Cord): Cord = cord(self <++> xs.self)

  /**
   * Appends a `String` to the end of this `Cord`.
   * Time complexity: O(1)
   */
  def :+(x: => String): Cord = cord(self :+ x)

  /**
   * Prepends a `String` to the beginning of this `Cord`.
   * Time complexity: O(1)
   */
  def +:(x: => String): Cord = cord(x +: self)

  /**
   * Prepends a `Char` to the beginning of this `Cord`.
   * Time complexity: O(1)
   */
  def -:(x: => Char): Cord = cord(x.toString +: self)

  /**
   * Appends a `Char` to the end of this `Cord`.
   * Time complexity: O(1)
   */
  def :-(x: => Char): Cord = cord(self :+ x.toString)

  /**
   * Removes the first character of this `Cord`.
   * Time complexity: O(1)
   */
  def tail: Cord = drop(1)

  /**
   * Removes the last character of this `Cord`.
   * Time complexity: O(1)
   */
  def init: Cord = take(self.measure - 1)

  /**
   * Removes the first `n` characters from the front of this `Cord`.
   * Time complexity: O(min N (N - n))
   */
  def drop(n: Int): Cord = split(n)._2

  /**
   * Returns the first `n` characters at the front of this `Cord`.
   * Time complexity: O(min N (N - n))
   */
  def take(n: Int): Cord = split(n)._1

  /**
   * Modifies each character in this `Cord` by the given function.
   * Time complexity: O(N)
   */
  def map[B](f: Char => Char): Cord = cord(self map (_ map f))

  def toList: List[Char] = toIndexedSeq.toList
  def toStream: Stream[Char] = toIndexedSeq.toStream
  def toIndexedSeq: IndexedSeq[Char] = self.foldMap(_.toIndexedSeq)
  override def toString: String = {
    val sb = new StringBuilder(self.measure)
    self foreach (sb ++= _)
    sb.toString
  }

  /** Transforms each character to a `Cord` according to the given function and concatenates them all into one `Cord`. */
  def flatMap[B](f: Char => Cord): Cord = toIndexedSeq.foldLeft(Cord())((as, a) => as ++ f(a))
}

object Cord {
  private def cord[A](v: FingerTree[Int, String]): Cord = new Cord {
    val self = v
  }

  implicit def stringToCord(s: String): Cord = cord(FingerTree.single[Int, String](s))

  lazy val empty: Cord = apply()

  def apply(as: Cord*): Cord = as.foldLeft(cord(FingerTree.empty))(_ ++ _)

  def fromStrings[A](as: Seq[String]): Cord = cord(as.foldLeft(FingerTree.empty[Int, String](sizer))((x, y) => x :+ y))

  implicit def sizer: Reducer[String, Int] = UnitReducer((a: String) => a.length)

  def mkCord(sep: Cord, as: Cord*): Cord =
    if (as.length > 0)
      as.tail.foldLeft(as.head)(_ ++ sep ++ _)
    else
      Cord()

  implicit lazy val CordShow: Show[Cord] = new Show[Cord] {
    override def show(x: Cord) = x
    override def shows(x: Cord) = x.toString
  }
  implicit lazy val CordMonoid: Monoid[Cord] = new Monoid[Cord] {
    def zero = empty
    def append(x: Cord, y: => Cord) = x ++ y
  }
  implicit lazy val CordEqual: Equal[Cord] = new Equal[Cord] {
    def equal(x: Cord, y: Cord) = Equal[FingerTree[Int, String]].equal(x.self, y.self)
  }
}
