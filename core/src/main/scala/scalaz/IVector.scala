/**
 Copyright (c) 2007-2008, Rich Hickey
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

 * Neither the name of Clojure nor the names of its contributors
   may be used to endorse or promote products derived from this
   software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
 **/

package scalaz

import scala.annotation.tailrec

import syntax.equal._
import Liskov._

/**
 * A straight port of Clojure's <code>PersistentIVector</code> class (with some
 * additional optimizations which may eventually land in Clojure's mainline).
 * For the record, this implementation is about 30% faster than
 * {@link scala.collection.immutable.Vector} on reads and about 5% slower for
 * "writes".
 *
 * Unlike the Scala stdlib Vector, prepend operations are very inefficient.  If
 * you want a data structure with efficient prepend, use IList.  The optimization
 * targets for this data structure are a) very fast append, and b) very fast
 * nat indexing.  Benchmarks suggest that these goals have been met to a level
 * that is generally better than the (more generalized) stdlib IVector for the
 * same operations.  Performance is especially good if *all* of the vectors in
 * your runtime have length < 1056 and you never perform calculations with the
 * empty vector.  In that use-case, nearly everything you do with IVector will
 * be aggressively inlined by HotSpot.
 *
 * At present, most utility functions are implemented in terms of foldLeft.  This
 * is quite inefficient due to the megamorphic call site.  We can do a lot better,
 * I'm just waiting to evaluate that thunk until someone needs it.
 *
 * @author Daniel Spiewak
 * @author Rich Hickey
 */
final class IVector[A] private (val length: Int, trie: IVectorCases.Case, tail: Array[AnyRef])
    extends Product
    with Serializable { outer =>

  import IVectorCases._

  private val tailOff = length - tail.length

  /*
   * The design of this data structure inherantly requires heterogenous arrays.
   * It is *possible* to design around this, but the result is comparatively
   * quite inefficient.  With respect to this fact, I have left the original
   * (somewhat dynamically-typed) implementation in place.
   */

  private def this() = this(0, IVectorCases.Zero, IVector.EmptyArray)

  def ++(that: IVector[A]): IVector[A] =
    that.foldLeft(this) { _ :+ _ }

  def :+(obj: A): IVector[A] = {
    if (tail.length < 32) {
      val tail2 = new Array[AnyRef](tail.length + 1)
      Array.copy(tail, 0, tail2, 0, tail.length)
      tail2(tail.length) = obj.asInstanceOf[AnyRef]

      new IVector[A](length + 1, trie, tail2)
    } else {
      new IVector[A](length + 1, trie + tail, IVector.array(obj.asInstanceOf[AnyRef]))
    }
  }

  // O(n), and not as good as it could be in the constant factors
  def +:(obj: A): IVector[A] = IVector(obj) ++ this

  /* alias for foldRight */
  def :\[B](b: B)(f: (A, B) => B): B = foldRight(b)(f)

  /* alias for foldLeft */
  def /:[B](b: B)(f: (B, A) => B): B = foldLeft(b)(f)

  /**
   * Unsafe dereference by integer index.  Throws an exception(!) when supplied
   * with an out-of-bounds index.  For a safer (but slower) dereference operation,
   * see `get`.
   */
  def apply(i: Int): A = {
    if (i >= 0 && i < length) {
      if (i >= tailOff) {
        tail(i & 0x01f).asInstanceOf[A]
      } else {
        var arr = trie(i)
        arr(i & 0x01f).asInstanceOf[A]
      }
    } else throw new IndexOutOfBoundsException(i.toString)
  }

  def collect[B](pf: PartialFunction[A, B]): IVector[B] = {
    foldLeft(IVector.empty[B]) { (acc, a) =>
      if (pf isDefinedAt a)
        acc :+ pf(a)
      else
        acc
    }
  }

  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] =
    find(pf isDefinedAt) map pf

  def containsSlice(that: IVector[A])(implicit ev: Equal[A]): Boolean =
    lastIndexOfSlice(that).isDefined

  def concat(that: IVector[A]): IVector[A] =
    this ++ that

  def count(f: A => Boolean): Int =
    foldLeft(0) { (n, a) => if (f(a)) n + 1 else n }

  def drop(n: Int): IVector[A] = {
    def inner(n: Int, acc: IVector[A]): IVector[A] = {
      if (n < length)
        inner(n + 1, acc :+ apply(n))
      else
        acc
    }

    if (n <= 0)
      this
    else
      inner(n, IVector.empty)
  }

  @tailrec
  def dropRight(n: Int): IVector[A] = {
    if (n <= 0)
      this
    else
      pop dropRight (n - 1)
  }

  @tailrec
  def dropRightWhile(f: A => Boolean): IVector[A] = {
    if (length > 0 && f(apply(length - 1)))
      pop dropRightWhile f
    else
      this
  }

  def dropWhile(f: A => Boolean): IVector[A] = {
    @tailrec
    def inner(i: Int): IVector[A] = {
      if (i < length && f(this(i)))
        inner(i + 1)
      else
        drop(i)
    }

    inner(0)
  }

  def endsWith(that: IVector[A])(implicit ev: Equal[A]): Boolean =
    matches(that, length - that.length)

  def filter(f: A => Boolean): IVector[A] =
    foldLeft(IVector.empty[A]) { (acc, a) => if (f(a)) acc :+ a else acc }

  // L2andThen
  def filterNot(f: A => Boolean): IVector[A] =
    filter { a => !f(a) }

  def find(f: A => Boolean): Option[A] = {
    @tailrec
    def inner(i: Int): Option[A] = {
      if (i >= length) {
        None
      } else {
        val a = apply(i)

        if (f(a))
          Some(a)
        else
          inner(i + 1)
      }
    }

    inner(0)
  }

  // but...but...CanBuildFrom!
  def flatMap[B](f: A => IVector[B]): IVector[B] =
    foldLeft(IVector.empty[B]) { _ ++ f(_) }

  def flatten[B](implicit ev: A <~< IVector[B]): IVector[B] = flatMap(ev)

  // TODO it's possible to more efficiently traverse the trie by delegating to Case
  def foldLeft[B](seed: B)(f: (B, A) => B): B = {
    @tailrec
    def inner(i: Int, seed: B): B = {
      if (i < length)
        inner(i + 1, f(seed, apply(i)))
      else
        seed
    }

    inner(0, seed)
  }

  // uses constant stack, because VECTOR POWAAAAAAAH!
  def foldRight[B](seed: B)(f: (A, B) => B): B = {
    @tailrec
    def inner(i: Int, seed: B): B = {
      if (i > 0)
        inner(i - 1, f(apply(i - 1), seed))
      else
        seed
    }

    inner(length, seed)
  }

  /**
   * Safe dereference operation.  Slower than apply due to the boxing into Option.
   */
  def get(i: Int): Option[A] =
    if (i >= 0 && i < length) Some(this(i)) else None

  def groupBy[K](f: A => K)(implicit ev: Order[K]): K ==>> IVector[A] = {
    foldLeft(==>>.empty[K, IVector[A]]) { (m, a) =>
      m.alter(f(a), { _ map { _ :+ a } orElse Some(IVector(a)) })
    }
  }

  def groupBy1[K](f: A => K)(implicit ev: Order[K]): K ==>> OneAnd[IVector, A] = {
    foldLeft(==>>.empty[K, OneAnd[IVector,A]]) { (m, a) =>
      m.alter(f(a), { _ map { oa => OneAnd(a, oa.tail :+ oa.head) } orElse Some(OneAnd(a, IVector.empty[A])) })
    }
  }

  def headOption: Option[A] =
    if (length > 0) Some(apply(0)) else None

  def headMaybe: Maybe[A] =
    if (length > 0) Maybe.Just(apply(0)) else Maybe.Empty()

  def indexOf(a: A)(implicit ev: Equal[A]): Option[Int] =
    indexWhere { _ === a }

  def indexOfSlice(that: IVector[A])(implicit ev: Equal[A]): Option[Int] = {
    @tailrec
    def inner(offset: Int): Option[Int] = {
      if (offset + that.length > length) {
        None
      } else {
        if (matches(that, offset))
          Some(offset)
        else
          inner(offset + 1)
      }
    }

    inner(0)
  }

  def indexWhere(f: A => Boolean): Option[Int] = {
    @tailrec
    def inner(i: Int): Option[Int] = {
      if (i >= length) {
        None
      } else {
        if (f(apply(i)))
          Some(i)
        else
          inner(i + 1)
      }
    }

    inner(0)
  }

  def initOption: Option[IVector[A]] =
    if (isEmpty) None else Some(pop)

  def inits: IVector[IVector[A]] = {
    @tailrec
    def inner(self: IVector[A], acc: IVector[IVector[A]]): IVector[IVector[A]] = {
      if (self.isEmpty)
        acc :+ self
      else
        inner(self.pop, acc :+ self)
    }

    inner(this, IVector.empty)
  }

  def intersperse(a: A): IVector[A] = {
    @tailrec
    def inner(i: Int, acc: IVector[A]): IVector[A] = {
      if (i < length)
        inner(i + 1, acc :+ a :+ apply(i))
      else
        acc
    }

    if (isEmpty)
      this
    else
      inner(1, IVector(apply(0)))
  }

  def isEmpty: Boolean = length == 0

  def lastIndexOf(a: A)(implicit ev: Equal[A]): Option[Int] =
    lastIndexWhere { a === _ }

  def lastIndexOfSlice(that: IVector[A])(implicit ev: Equal[A]): Option[Int] = {
    @tailrec
    def inner(offset: Int): Option[Int] = {
      if (offset < 0) {
        None
      } else {
        if (matches(that, offset))
          Some(offset)
        else
          inner(offset - 1)
      }
    }

    inner(length - that.length)
  }

  def lastIndexWhere(f: A => Boolean): Option[Int] = {
    @tailrec
    def inner(i: Int): Option[Int] = {
      if (i < 0) {
        None
      } else {
        if (f(apply(i)))
          Some(i)
        else
          inner(i - 1)
      }
    }

    inner(length - 1)
  }

  def lastOption: Option[A] =
    if (isEmpty) None else Some(this(length - 1))

  def map[B](f: A => B): IVector[B] =
    foldLeft(IVector.empty[B]) { _ :+ f(_) }

  def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, IVector[B]) = {
    @tailrec
    def inner(c: C, index: Int, acc: IVector[B]): (C, IVector[B]) = {
      if (index >= length) {
        (c, acc)
      } else {
        val (c2, b) = f(c, this(index))
        inner(c2, index + 1, acc :+ b)
      }
    }

    inner(c, 0, IVector.empty)
  }

  def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, IVector[B]) = {
    @tailrec
    def inner(c: C, index: Int, acc: IVector[B]): (C, IVector[B]) = {
      if (index < 0) {
        (c, acc.reverse)
      } else {
        val (c2, b) = f(c, this(index))
        inner(c2, index - 1, acc :+ b)
      }
    }

    inner(c, length - 1, IVector.empty)
  }

  // endsWith v = matches(v, length - v.length)
  // startsWith v = matches(v, 0)
  // could probably be made more efficient delegating to Case
  def matches(that: IVector[A], offset: Int)(implicit ev: Equal[A]): Boolean = {
    @tailrec
    def inner(i: Int, acc: Boolean): Boolean = {
      if (!acc || i + offset >= this.length || i >= that.length)
        acc
      else
        inner(i + 1, this(i + offset) === that(i))
    }

    if (that.isEmpty)
      true
    else if (offset < 0)
      false
    else if (offset > length)
      false
    else if (this.length < that.length + offset)
      false
    else
      inner(0, true)
  }

  // ugggghhhh, I hate functions like this
  def nonEmpty: Boolean = !isEmpty

  @tailrec
  def padTo(n: Int, a: A): IVector[A] = {
    if (n <= length)
      this
    else
      (this :+ a).padTo(n, a)
  }

  def partition(f: A => Boolean): (IVector[A], IVector[A]) =
    indexWhere(f) map splitAt getOrElse ((this, IVector.empty[A]))

  def patch(from: Int, patch: IVector[A], replaced: Int): IVector[A] = {
    val (init, tail) = splitAt(from)
    init ++ patch ++ (tail drop replaced)
  }

  /**
   * Removes the <i>tail</i> element of this vector.  Equivalent to dropRight(1)
   */
  def pop: IVector[A] = {
    if (isEmpty) {
      this
    } else if (length == 1) {
      IVector.empty
    } else if (tail.length > 1) {
      val tail2 = new Array[AnyRef](tail.length - 1)
      Array.copy(tail, 0, tail2, 0, tail2.length)

      new IVector[A](length - 1, trie, tail2)
    } else {
      val (trie2, tail2) = trie.pop
      new IVector[A](length - 1, trie2, tail2)
    }
  }

  def prefixLength(f: A => Boolean): Int = {
    @tailrec
    def inner(index: Int): Int = {
      if (index >= length) {
        index
      } else {
        if (f(this(index)))
          inner(index + 1)
        else
          index
      }
    }

    inner(0)
  }

  def reduceLeftOption(f: (A, A) => A): Option[A] = {
    @tailrec
    def inner(index: Int, a: A): A = {
      if (index >= length)
        a
      else
        inner(index + 1, f(a, this(index)))
    }

    if (length <= 0)
      None
    else
      Some(inner(1, this(0)))
  }

  def reduceRightOption(f: (A, A) => A): Option[A] = {
    @tailrec
    def inner(index: Int, a: A): A = {
      if (index < 0)
        a
      else
        inner(index - 1, f(a, this(index)))
    }

    if (length <= 0)
      None
    else
      Some(inner(length - 2, this(length - 1)))
  }

  def reverse: IVector[A] = {
    @tailrec
    def inner(index: Int, acc: IVector[A]): IVector[A] = {
      if (index < 0)
        acc
      else
        inner(index - 1, acc :+ this(index))
    }

    inner(length - 1, IVector.empty)
  }

  def reverseMap[B](f: A => B): IVector[B] = {
    @tailrec
    def inner(index: Int, acc: IVector[B]): IVector[B] = {
      if (index < 0)
        acc
      else
        inner(index - 1, acc :+ f(this(index)))
    }

    inner(length - 1, IVector.empty)
  }

  // TODO it's possible to more efficiently traverse the trie by delegating to Case
  def scanLeft[B](seed: B)(f: (B, A) => B): IVector[B] = {
    @tailrec
    def inner(i: Int, seed: B, acc: IVector[B]): IVector[B] = {
      if (i < length) {
        val next = f(seed, apply(i))
        inner(i + 1, next, acc :+ next)
      } else {
        acc
      }
    }

    inner(0, seed, IVector(seed))
  }

  def scanRight[B](seed: B)(f: (A, B) => B): IVector[B] = {
    @tailrec
    def inner(i: Int, seed: B, acc: IVector[B]): IVector[B] = {
      if (i > 0) {
        val next = f(apply(i - 1), seed)
        inner(i - 1, next, acc :+ next)
      } else {
        acc.reverse
      }
    }

    inner(length, seed, IVector(seed))
  }

  def slice(from: Int, until: Int): IVector[A] =
    this drop from take ((until max 0) - (from max 0))

  def sortBy[B](f: A => B)(implicit B: Order[B]): IVector[A] =
    IVector(toList.sortBy(f)(B.toScalaOrdering): _*)     // oh yeah, we can totally do better here...

  def sorted(implicit ev: Order[A]): IVector[A] = sortBy(identity)

  def span(f: A => Boolean): (IVector[A], IVector[A]) = {
    @tailrec
    def inner(index: Int, acc: IVector[A]): (IVector[A], IVector[A]) = {
      if (index >= length) {
        (acc, IVector.empty)
      } else {
        val a = this(index)

        if (f(a))
          inner(index + 1, acc :+ a)
        else
          (acc, this drop index)
      }
    }

    inner(0, IVector.empty)
  }

  def splitAt(at: Int): (IVector[A], IVector[A]) = {
    @tailrec
    def inner(index: Int, acc: IVector[A]): (IVector[A], IVector[A]) = {
      if (index >= length) {
        (acc, IVector.empty)
      } else {
        if (index < at)
          inner(index + 1, acc :+ this(index))
        else
          (acc, this drop index)
      }
    }

    inner(0, IVector.empty)
  }

  def startsWith(that: IVector[A])(implicit ev: Equal[A]): Boolean =
    matches(that, 0)

  /**
   * This function isn't actually as efficient as you would expect, since elements
   * are popped from the _head_ rather than the tail.  Thus, inits is O(n log_32 n),
   * while tails is O(n^2).
   */
  def tails: IVector[IVector[A]] = {
    @tailrec
    def inner(index: Int, acc: IVector[IVector[A]]): IVector[IVector[A]] = {
      if (index > length)
        acc
      else
        inner(index + 1, acc :+ drop(index))
    }

    inner(0, IVector.empty)
  }

  def tailOption: Option[IVector[A]] = {
    if (length == 0)
      None
    else
      Some(this drop 1)
  }

  def take(n: Int): IVector[A] =
    dropRight(length - n)

  def takeRight(n: Int): IVector[A] = drop((length - n) max 0)

  def takeRightWhile(f: A => Boolean): IVector[A] = {
    @tailrec
    def inner(index: Int, acc: IVector[A]): IVector[A] = {
      if (index < 0) {
        acc.reverse
      } else {
        val a = this(index)

        if (f(a))
          inner(index - 1, acc :+ a)
        else
          acc.reverse
      }
    }

    inner(length - 1, IVector.empty)
  }

  def takeWhile(f: A => Boolean): IVector[A] = {
    @tailrec
    def inner(index: Int, acc: IVector[A]): IVector[A] = {
      if (index >= length) {
        acc
      } else {
        val a = this(index)

        if (f(a))
          inner(index + 1, acc :+ a)
        else
          acc
      }
    }

    inner(0, IVector.empty)
  }

  def toEphemeralStream: EphemeralStream[A] = {
    def inner(index: Int): EphemeralStream[A] = {
      if (index >= length)
        EphemeralStream()
      else
        EphemeralStream.cons(this(index), inner(index + 1))
    }

    inner(0)
  }

  def toList: List[A] = foldRight(Nil: List[A]) { _ :: _ }

  def toNel: Option[NonEmptyList[A]] = {
    if (isEmpty)
      None
    else
      Some(NonEmptyList.nel(this(0), toList.tail))
  }

  def toMap[K, V](implicit ev0: A <~< (K, V), ev1: Order[K]): K ==>> V =
    widen[(K, V)].foldLeft(==>>.empty[K, V]) { _ + _ }

  def toStream: Stream[A] = {
    def inner(index: Int): Stream[A] = {
      if (index >= length)
        Stream.empty
      else
        this(index) #:: inner(index + 1)
    }

    inner(0)
  }

  override def toString: String =
    IVector show Show.showA shows this

  // heads up! it's still technically faster to append to scala.Vector
  def toVector: Vector[A] = foldLeft(Vector[A]()) { _ :+ _}

  def toZipper: Option[Zipper[A]] = std.stream.toZipper(toStream)

  def unzip[B, C](implicit ev: A <~< (B, C)): (IVector[B], IVector[C]) = {
    widen[(B, C)].foldLeft((IVector.empty[B], IVector.empty[C])) {
      case ((bs, cs), (b, c)) => (bs :+ b, cs :+ c)
    }
  }

  def updated(i: Int, obj: A): IVector[A] = {
    if (i >= 0 && i < length) {
      if (i >= tailOff) {
        val newTail = new Array[AnyRef](tail.length)
        Array.copy(tail, 0, newTail, 0, tail.length)
        newTail(i & 0x01f) = obj.asInstanceOf[AnyRef]

        new IVector[A](length, trie, newTail)
      } else {
        new IVector[A](length, trie(i) = obj.asInstanceOf[AnyRef], tail)
      }
    } else {
      this
    }
  }

  def widen[B](implicit ev: A <~< B): IVector[B] =
    this.asInstanceOf[IVector[B]]     // protip!  this is actually sound and doesn't involve type lambdas

  def zip[B](_that: => IVector[B]): IVector[(A, B)] = {
    lazy val that = _that

    @tailrec
    def inner(index: Int, acc: IVector[(A, B)]): IVector[(A, B)] = {
      if (index >= this.length || index >= that.length)
        acc
      else
        inner(index + 1, acc :+ ((this(index), that(index))))
    }

    inner(0, IVector.empty)
  }

  def zipWithIndex: IVector[(A, Int)] = {
    @tailrec
    def inner(index: Int, acc: IVector[(A, Int)]): IVector[(A, Int)] = {
      if (index >= length)
        acc
      else
        inner(index + 1, acc :+ ((this(index), index)))
    }

    inner(0, IVector.empty)
  }

  // dumb product stuff...

  def canEqual(that: Any): Boolean = that match {
    case that: IVector[_] => that.length == this.length
    case _ => false
  }

  def productArity: Int = length

  def productElement(n: Int): Any = apply(n)
}

/*
 * TODO we might be able to do something extremely clever with exhaustiveness
 * checking by collapsing IVectorCases into IVector.  It's gonna be super-ugly
 */
object :+ {
  def unapply[A](vec: IVector[A]): Option[(IVector[A], A)] =
    if (vec.length > 0) Some((vec.pop, vec(vec.length - 1))) else None
}

object IVector extends IVectorInstances {
  private val EmptyArray = new Array[AnyRef](0)

  private[this] val EmptyIVector = new IVector[Nothing]

  def empty[A]: IVector[A] = EmptyIVector.asInstanceOf[IVector[A]]

  // TODO more efficient implementation
  def apply[A](elems: A*): IVector[A] = elems.foldLeft(empty[A]) { _ :+ _ }

  def fill[A](n: Int)(a: A): IVector[A] = IVector.empty[A].padTo(n, a)

  /**
   * You probably shouldn't use this function, since it's a lot less efficient
   * than you would expect.  It's mostly here for convenience.  The :+ deconstructor
   * is much faster.
   */
  def unapply[A](vec: IVector[A]): Seq[A] = vec.foldLeft(Vector[A]()) { _ :+ _ }

  private def array(elem: AnyRef) = {
    val back = new Array[AnyRef](1)
    back(0) = elem
    back
  }
}

// TODO split up functions into inlineable chunks
private object IVectorCases {

  private[this] def copy1(array1: Array[AnyRef], array2: Array[AnyRef]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }

  private[this] def copy2(array1: Array[Array[AnyRef]], array2: Array[Array[AnyRef]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }

  private[this] def copy3(array1: Array[Array[Array[AnyRef]]], array2: Array[Array[Array[AnyRef]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }

  private[this] def copy4(array1: Array[Array[Array[Array[AnyRef]]]], array2: Array[Array[Array[Array[AnyRef]]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }

  private[this] def copy5(array1: Array[Array[Array[Array[Array[AnyRef]]]]], array2: Array[Array[Array[Array[Array[AnyRef]]]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }

  private[this] def copy6(array1: Array[Array[Array[Array[Array[Array[AnyRef]]]]]], array2: Array[Array[Array[Array[Array[Array[AnyRef]]]]]]) = {
    Array.copy(array1, 0, array2, 0, Math.min(array1.length, array2.length))
    array2
  }

  sealed trait Case {
    type Self <: Case

    val shift: Int

    def apply(i: Int): Array[AnyRef]
    def update(i: Int, obj: AnyRef): Self

    def +(node: Array[AnyRef]): Case
    def pop: (Case, Array[AnyRef])
  }

  case object Zero extends Case {
    type Self = Nothing

    val shift = -1

    def apply(i: Int) = throw new IndexOutOfBoundsException(i.toString)
    def update(i: Int, obj: AnyRef) = throw new IndexOutOfBoundsException(i.toString)

    def +(node: Array[AnyRef]) = One(node)
    def pop = throw new IndexOutOfBoundsException("Cannot pop an empty IVector")
  }

  case class One(trie: Array[AnyRef]) extends Case {
    type Self = One

    val shift = 0

    def apply(i: Int) = trie

    def update(i: Int, obj: AnyRef) = {
      val trie2 = copy1(trie, new Array[AnyRef](trie.length))
      trie2(i & 0x01f) = obj
      One(trie2)
    }

    def +(tail: Array[AnyRef]) = {
      val trie2 = new Array[Array[AnyRef]](2)
      trie2(0) = trie
      trie2(1) = tail
      Two(trie2)
    }

    def pop = (Zero, trie)
  }

  case class Two(trie: Array[Array[AnyRef]]) extends Case {
    type Self = Two

    val shift = 5

    def apply(i: Int) = trie((i >>> 5) & 0x01f)

    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy2(trie, new Array[Array[AnyRef]](trie.length))

      val trie2b = {
        val target = trie2a((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2a((i >>> 5) & 0x01f) = trie2b

      trie2b(i & 0x01f) = obj
      Two(trie2a)
    }

    def +(tail: Array[AnyRef]) = {
      if (trie.length >= 32) {
        val trie2 = new Array[Array[Array[AnyRef]]](2)
        trie2(0) = trie

        trie2(1) = new Array[Array[AnyRef]](1)
        trie2(1)(0) = tail

        Three(trie2)
      } else {
        val trie2 = copy2(trie, new Array[Array[AnyRef]](trie.length + 1))
        trie2(trie.length) = tail
        Two(trie2)
      }
    }

    def pop = {
      if (trie.length == 2) {
        (One(trie(0)), trie.last)
      } else {
        val trie2 = copy2(trie, new Array[Array[AnyRef]](trie.length - 1))
        (Two(trie2), trie.last)
      }
    }
  }

  case class Three(trie: Array[Array[Array[AnyRef]]]) extends Case {
    type Self = Three

    val shift = 10

    def apply(i: Int) = {
      val a = trie((i >>> 10) & 0x01f)
      a((i >>> 5) & 0x01f)
    }

    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length))

      val trie2b = {
        val target = trie2a((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2a((i >>> 10) & 0x01f) = trie2b

      val trie2c = {
        val target = trie2b((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2b((i >>> 5) & 0x01f) = trie2c

      trie2c(i & 0x01f) = obj
      Three(trie2a)
    }

    def +(tail: Array[AnyRef]) = {
      if (trie.last.length >= 32) {
        if (trie.length >= 32) {
          val trie2 = new Array[Array[Array[Array[AnyRef]]]](2)
          trie2(0) = trie

          trie2(1) = new Array[Array[Array[AnyRef]]](1)
          trie2(1)(0) = new Array[Array[AnyRef]](1)
          trie2(1)(0)(0) = tail

          Four(trie2)
        } else {
          val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length + 1))
          trie2(trie.length) = new Array[Array[AnyRef]](1)
          trie2(trie.length)(0) = tail
          Three(trie2)
        }
      } else {
        val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length))
        trie2(trie2.length - 1) = copy2(trie2.last, new Array[Array[AnyRef]](trie2.last.length + 1))
        trie2.last(trie.last.length) = tail
        Three(trie2)
      }
    }

    def pop = {
      if (trie.last.length == 1) {
        if (trie.length == 2) {
          (Two(trie(0)), trie.last.last)
        } else {
          val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length - 1))
          (Three(trie2), trie.last.last)
        }
      } else {
        val trie2 = copy3(trie, new Array[Array[Array[AnyRef]]](trie.length))
        trie2(trie2.length - 1) = copy2(trie2.last, new Array[Array[AnyRef]](trie2.last.length - 1))
        (Three(trie2), trie.last.last)
      }
    }
  }

  case class Four(trie: Array[Array[Array[Array[AnyRef]]]]) extends Case {
    type Self = Four

    val shift = 15

    def apply(i: Int) = {
      val a = trie((i >>> 15) & 0x01f)
      val b = a((i >>> 10) & 0x01f)
      b((i >>> 5) & 0x01f)
    }

    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))

      val trie2b = {
        val target = trie2a((i >>> 15) & 0x01f)
        copy3(target, new Array[Array[Array[AnyRef]]](target.length))
      }
      trie2a((i >>> 15) & 0x01f) = trie2b

      val trie2c = {
        val target = trie2b((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2b((i >>> 10) & 0x01f) = trie2c

      val trie2d = {
        val target = trie2c((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2c((i >>> 5) & 0x01f) = trie2d

      trie2d(i & 0x01f) = obj
      Four(trie2a)
    }

    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.length >= 32) {
        if (trie.last.length >= 32) {
          if (trie.length >= 32) {
            val trie2 = new Array[Array[Array[Array[Array[AnyRef]]]]](2)
            trie2(0) = trie

            trie2(1) = new Array[Array[Array[Array[AnyRef]]]](1)
            trie2(1)(0) = new Array[Array[Array[AnyRef]]](1)
            trie2(1)(0)(0) = new Array[Array[AnyRef]](1)
            trie2(1)(0)(0)(0) = tail

            Five(trie2)
          } else {
            val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length + 1))
            trie2(trie.length) = new Array[Array[Array[AnyRef]]](1)
            trie2(trie.length)(0) = new Array[Array[AnyRef]](1)
            trie2(trie.length)(0)(0) = tail
            Four(trie2)
          }
        } else {
          val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
          trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length + 1))
          trie2.last(trie.last.length) = new Array[Array[AnyRef]](1)
          trie2.last.last(0) = tail
          Four(trie2)
        }
      } else {
        val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
        trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length))
        trie2.last(trie2.last.length - 1) = copy2(trie2.last.last, new Array[Array[AnyRef]](trie2.last.last.length + 1))
        trie2.last.last(trie.last.last.length) = tail
        Four(trie2)
      }
    }

    def pop = {
      if (trie.last.last.length == 1) {
        if (trie.last.length == 1) {
          if (trie.length == 2) {
            (Three(trie(0)), trie.last.last.last)
          } else {
            val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length - 1))
            (Four(trie2), trie.last.last.last)
          }
        } else {
          val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
          trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length - 1))
          (Four(trie2), trie.last.last.last)
        }
      } else {
        val trie2 = copy4(trie, new Array[Array[Array[Array[AnyRef]]]](trie.length))
        trie2(trie2.length - 1) = copy3(trie2.last, new Array[Array[Array[AnyRef]]](trie2.last.length - 1))
        trie2.last(trie2.last.length - 1) = copy2(trie2.last.last, new Array[Array[AnyRef]](trie2.last.last.length - 1))
        (Four(trie2), trie.last.last.last)
      }
    }
  }

  case class Five(trie: Array[Array[Array[Array[Array[AnyRef]]]]]) extends Case {
    type Self = Five

    val shift = 20

    def apply(i: Int) = {
      val a = trie((i >>> 20) & 0x01f)
      val b = a((i >>> 15) & 0x01f)
      val c = b((i >>> 10) & 0x01f)
      c((i >>> 5) & 0x01f)
    }

    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))

      val trie2b = {
        val target = trie2a((i >>> 20) & 0x01f)
        copy4(target, new Array[Array[Array[Array[AnyRef]]]](target.length))
      }
      trie2a((i >>> 20) & 0x01f) = trie2b

      val trie2c = {
        val target = trie2b((i >>> 15) & 0x01f)
        copy3(target, new Array[Array[Array[AnyRef]]](target.length))
      }
      trie2b((i >>> 15) & 0x01f) = trie2c

      val trie2d = {
        val target = trie2c((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2c((i >>> 10) & 0x01f) = trie2d

      val trie2e = {
        val target = trie2d((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2d((i >>> 5) & 0x01f) = trie2e

      trie2e(i & 0x01f) = obj
      Five(trie2a)
    }

    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.last.length >= 32) {
        if (trie.last.last.length >= 32) {
          if (trie.last.length >= 32) {
            if (trie.length >= 32) {
              val trie2 = new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](2)
              trie2(0) = trie

              trie2(1) = new Array[Array[Array[Array[Array[AnyRef]]]]](1)
              trie2(1)(0) = new Array[Array[Array[Array[AnyRef]]]](1)
              trie2(1)(0)(0) = new Array[Array[Array[AnyRef]]](1)
              trie2(1)(0)(0)(0) = new Array[Array[AnyRef]](1)
              trie2(1)(0)(0)(0)(0) = tail

              Six(trie2)
            } else {
              val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length + 1))
              trie2(trie.length) = new Array[Array[Array[Array[AnyRef]]]](1)
              trie2(trie.length)(0) = new Array[Array[Array[AnyRef]]](1)
              trie2(trie.length)(0)(0) = new Array[Array[AnyRef]](1)
              trie2(trie.length)(0)(0)(0) = tail
              Five(trie2)
            }
          } else {
            val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
            trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length + 1))
            trie2.last(trie.last.length) = new Array[Array[Array[AnyRef]]](1)
            trie2.last.last(0) = new Array[Array[AnyRef]](1)
            trie2.last.last.last(0) = tail
            Five(trie2)
          }
        } else {
          val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
          trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length))
          trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length + 1))
          trie2.last.last(trie.last.last.length) = new Array[Array[AnyRef]](1)
          trie2.last.last.last(0) = tail
          Five(trie2)
        }
      } else {
        val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
        trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length))
        trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length))
        trie2.last.last(trie2.last.last.length - 1) = copy2(trie2.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.length + 1))
        trie2.last.last.last(trie.last.last.last.length) = tail
        Five(trie2)
      }
    }

    def pop = {
      if (trie.last.last.last.length == 1) {
        if (trie.last.last.length == 1) {
          if (trie.last.length == 1) {
            if (trie.length == 2) {
              (Four(trie(0)), trie.last.last.last.last)
            } else {
              val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length - 1))
              (Five(trie2), trie.last.last.last.last)
            }
          } else {
            val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
            trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length - 1))
            (Five(trie2), trie.last.last.last.last)
          }
        } else {
          val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
          trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length - 1))
          trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length - 1))
          (Five(trie2), trie.last.last.last.last)
        }
      } else {
        val trie2 = copy5(trie, new Array[Array[Array[Array[Array[AnyRef]]]]](trie.length))
        trie2(trie2.length - 1) = copy4(trie2.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.length - 1))
        trie2.last(trie2.last.length - 1) = copy3(trie2.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.length - 1))
        trie2.last.last(trie2.last.last.length - 1) = copy2(trie2.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.length - 1))
        (Five(trie2), trie.last.last.last.last)
      }
    }
  }

  case class Six(trie: Array[Array[Array[Array[Array[Array[AnyRef]]]]]]) extends Case {
    type Self = Six

    val shift = 25

    def apply(i: Int) = {
      val a = trie((i >>> 25) & 0x01f)
      val b = a((i >>> 20) & 0x01f)
      val c = b((i >>> 15) & 0x01f)
      val d = c((i >>> 10) & 0x01f)
      d((i >>> 5) & 0x01f)
    }

    def update(i: Int, obj: AnyRef) = {
      val trie2a = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))

      val trie2b = {
        val target = trie2a((i >>> 25) & 0x01f)
        copy5(target, new Array[Array[Array[Array[Array[AnyRef]]]]](target.length))
      }
      trie2a((i >>> 25) & 0x01f) = trie2b

      val trie2c = {
        val target = trie2b((i >>> 20) & 0x01f)
        copy4(target, new Array[Array[Array[Array[AnyRef]]]](target.length))
      }
      trie2b((i >>> 20) & 0x01f) = trie2c

      val trie2d = {
        val target = trie2c((i >>> 15) & 0x01f)
        copy3(target, new Array[Array[Array[AnyRef]]](target.length))
      }
      trie2c((i >>> 15) & 0x01f) = trie2d

      val trie2e = {
        val target = trie2d((i >>> 10) & 0x01f)
        copy2(target, new Array[Array[AnyRef]](target.length))
      }
      trie2d((i >>> 10) & 0x01f) = trie2e

      val trie2f = {
        val target = trie2e((i >>> 5) & 0x01f)
        copy1(target, new Array[AnyRef](target.length))
      }
      trie2e((i >>> 5) & 0x01f) = trie2f

      trie2f(i & 0x01f) = obj
      Six(trie2a)
    }

    def +(tail: Array[AnyRef]) = {
      if (trie.last.last.last.last.length >= 32) {
        if (trie.last.last.last.length >= 32) {
          if (trie.last.last.length >= 32) {
            if (trie.last.length >= 32) {
              if (trie.length >= 32) {
                throw new IndexOutOfBoundsException("Cannot grow vector beyond integer bounds")
              } else {
                val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length + 1))
                trie2(trie.length) = new Array[Array[Array[Array[Array[AnyRef]]]]](1)
                trie2(trie.length)(0) = new Array[Array[Array[Array[AnyRef]]]](1)
                trie2(trie.length)(0)(0) = new Array[Array[Array[AnyRef]]](1)
                trie2(trie.length)(0)(0)(0) = new Array[Array[AnyRef]](1)
                trie2(trie.length)(0)(0)(0)(0) = tail
                Six(trie2)
              }
            } else {
              val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
              trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length + 1))
              trie2.last(trie.last.length) = new Array[Array[Array[Array[AnyRef]]]](1)
              trie2.last.last(0) = new Array[Array[Array[AnyRef]]](1)
              trie2.last.last.last(0) = new Array[Array[AnyRef]](1)
              trie2.last.last.last.last(0) = tail
              Six(trie2)
            }
          } else {
            val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
            trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length))
            trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length + 1))
            trie2.last.last(trie.last.last.length) = new Array[Array[Array[AnyRef]]](1)
            trie2.last.last.last(0) = new Array[Array[AnyRef]](1)
            trie2.last.last.last.last(0) = tail
            Six(trie2)
          }
        } else {
          val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
          trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length))
          trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length))
          trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length + 1))
          trie2.last.last.last(trie.last.last.last.length) = new Array[Array[AnyRef]](1)
          trie2.last.last.last.last(0) = tail
          Six(trie2)
        }
      } else {
        val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
        trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length))
        trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length))
        trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length))
        trie2.last.last.last(trie.last.last.last.length - 1) = copy2(trie2.last.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.last.length + 1))
        trie2.last.last.last.last(trie.last.last.last.last.length) = tail
        Six(trie2)
      }
    }

    def pop = {
      if (trie.last.last.last.last.length == 1) {
        if (trie.last.last.last.length == 1) {
          if (trie.last.last.length == 1) {
            if (trie.last.length == 1) {
              if (trie.length == 2) {
                (Five(trie(0)), trie.last.last.last.last.last)
              } else {
                val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length - 1))
                (Six(trie2), trie.last.last.last.last.last)
              }
            } else {
              val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
              trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
              (Six(trie2), trie.last.last.last.last.last)
            }
          } else {
            val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
            trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
            trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length - 1))
            (Six(trie2), trie.last.last.last.last.last)
          }
        } else {
          val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
          trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
          trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length - 1))
          trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length - 1))
          (Six(trie2), trie.last.last.last.last.last)
        }
      } else {
        val trie2 = copy6(trie, new Array[Array[Array[Array[Array[Array[AnyRef]]]]]](trie.length))
        trie2(trie2.length - 1) = copy5(trie2.last, new Array[Array[Array[Array[Array[AnyRef]]]]](trie2.last.length - 1))
        trie2.last(trie2.last.length - 1) = copy4(trie2.last.last, new Array[Array[Array[Array[AnyRef]]]](trie2.last.last.length - 1))
        trie2.last.last(trie2.last.last.length - 1) = copy3(trie2.last.last.last, new Array[Array[Array[AnyRef]]](trie2.last.last.last.length - 1))
        trie2.last.last.last(trie2.last.last.last.length - 1) = copy2(trie2.last.last.last.last, new Array[Array[AnyRef]](trie2.last.last.last.last.length - 1))
        (Six(trie2), trie.last.last.last.last.last)
      }
    }
  }
}

// instances

sealed abstract class IVectorInstance0 {

  implicit def equal[A: Equal]: Equal[IVector[A]] =
    new IVectorEqual[A]
}

sealed abstract class IVectorInstances extends IVectorInstance0 {

  implicit val instances: Traverse[IVector] with MonadPlus[IVector] with Zip[IVector] with Unzip[IVector] with Align[IVector] with IsEmpty[IVector] with Cobind[IVector] = {
    new Traverse[IVector] with MonadPlus[IVector] with Zip[IVector] with Unzip[IVector] with Align[IVector] with IsEmpty[IVector] with Cobind[IVector] {

      def alignWith[A, B, C](f: A \&/ B => C): (IVector[A], IVector[B]) => IVector[C] = {
        @tailrec
        def loop(aa: IVector[A], bb: IVector[B], index: Int, acc: IVector[C]): IVector[C] = {
          if (index < aa.length && index < bb.length)
            loop(aa, bb, index + 1, acc :+ f(\&/.Both(aa(index), bb(index))))
          else if (index < aa.length)
            acc ++ (aa drop index map { a => f(\&/.This(a)) })
          else if (index < bb.length)
            acc ++ (bb drop index map { b => f(\&/.That(b)) })
          else
            acc
        }

        { (aa, bb) => loop(aa, bb, 0, IVector.empty) }
      }

      def point[A](a: => A): IVector[A] = IVector(a)

      def bind[A, B](fa: IVector[A])(f: A => IVector[B]): IVector[B] =
        fa flatMap f

      // not tail recursive.  the alternative involves reverse.  eww?
      def cobind[A, B](fa: IVector[A])(f: IVector[A] => B): IVector[B] = {
        if (fa.isEmpty)
          IVector.empty
        else
          cobind(fa.pop)(f) :+ f(fa)
      }

      def isEmpty[A](fa: IVector[A]): Boolean = fa.isEmpty

      def plus[A](a: IVector[A], b: => IVector[A]): IVector[A] =
        a ++ b

      def empty[A]: IVector[A] = IVector.empty[A]

      def traverseImpl[G[_], A, B](fa: IVector[A])(f: A => G[B])(implicit G: Applicative[G]): G[IVector[B]] =
        fa.foldLeft(G point IVector.empty[B]) { (fbs, a) => G.apply2(fbs, f(a)) { _ :+ _ } }

      def unzip[A, B](a: IVector[(A, B)]): (IVector[A], IVector[B]) = a.unzip

      def zip[A, B](a: => IVector[A], b: => IVector[B]): IVector[(A, B)] = a zip b

      override def foldRight[A, B](fa: IVector[A], z: => B)(f: (A, => B) => B) =
        fa.foldRight(z) { (a, b) => f(a, b) }

      override def foldMapRight1Opt[A, B](fa: IVector[A])(z: A => B)(f: (A, => B) => B) =
        foldMapLeft1Opt(fa.reverse)(z) { (b, a) => f(a, b) }

      override def foldMap[A, B](fa: IVector[A])(f: A => B)(implicit M: Monoid[B]) =
        fa.foldLeft(M.zero) { (b, a) => M.append(b, f(a)) }
    }
  }

  implicit def order[A: Order]: Order[IVector[A]] =
    new IVectorOrder[A]

  implicit def monoid[A]: Monoid[IVector[A]] = new Monoid[IVector[A]] {

    def append(f1: IVector[A], f2: => IVector[A]): IVector[A] =
      f1 ++ f2

    def zero: IVector[A] = IVector.empty
  }

  implicit def show[A: Show]: Show[IVector[A]] = new Show[IVector[A]] {
    override def show(as: IVector[A]) = {
      val A = Show[A]

      @tailrec
      def commaSep(index: Int, acc: Cord): Cord = {
        if (index >= as.length)
          acc
        else
          commaSep(index + 1, (acc :+ ",") ++ (A show as(index)))
      }

      val cord = if (as.isEmpty)
        Cord()
      else
        commaSep(1, A show as(0))

      "<" +: cord :+ ">"
    }
  }
}

private class IVectorEqual[A](implicit A: Equal[A]) extends Equal[IVector[A]] {
  def equal(a: IVector[A], b: IVector[A]): Boolean = {
    if (a.length == b.length) {
      @tailrec
      def inner(index: Int): Boolean = {
        if (index >= a.length)
          true
        else
          (a(index) === b(index)) && inner(index + 1)
      }

      inner(0)
    } else {
      false
    }
  }
}

private class IVectorOrder[A](implicit A: Order[A]) extends Order[IVector[A]] {
  import Ordering._

  def order(a: IVector[A], b: IVector[A]): Ordering = {
    if (a.length == b.length) {
      @tailrec
      def inner(index: Int): Ordering = {
        if (index >= a.length)
          EQ
        else {
          A.order(a(index), b(index)) match {
            case EQ => inner(index + 1)
            case x => x
          }
        }
      }

      inner(0)
    } else if (a.length > b.length) {
      GT
    } else {
      LT
    }
  }
}
