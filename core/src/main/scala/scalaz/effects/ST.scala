package scalaz
package effects

import Scalaz._

private[effects] case class World[A]()
sealed trait RealWorld

/** Mutable variable in state thread S containing a value of type A. */
class STRef[S, A](a: A) {
  private var value: A = a

  /** Reads the value pointed at by this reference. */
  def read: ST[S, A] = returnST(value)

  /** Modifies the value at this reference with the given function. */
  def mod[B](f: A => A): ST[S, STRef[S, A]] = ST((s: World[S]) => {value = f(value); (s, this)})

  /** Associates this reference with the given value. */
  def write(a: => A): ST[S, STRef[S, A]] = ST((s: World[S]) => {value = a; (s, this)})

  /** Swap the value at this reference with the value at another. */
  def swap(that: STRef[S, A]): ST[S, Unit] = for {
    v1 <- this.read
    v2 <- that.read
    _ <- this write v2
    _ <- that write v1
  } yield ()
}

/** Mutable array in state thread S containing values of type A. */
class STArray[S, A:Manifest](val size: Int, z: A) {
  private val value: Array[A] = Array.fill(size)(z)

  /** Reads the value at the given index. */
  def read(i: Int): ST[S, A] = returnST(value(i))

  /** Writes the given value to the array, at the given offset. */
  def write(i: Int, a: A): ST[S, STArray[S, A]] = ST(s => {value(i) = a; (s, this)})

  /** Turns a mutable array into an immutable one which is safe to return. */
  def freeze: ST[S, ImmutableArray[A]] = ST(s => (s, ImmutableArray.fromArray(value)))

  /** Fill this array from the given association list. */
  def fill[B](f: (A, B) => A, xs: Traversable[(Int, B)]): ST[S, Unit] = xs match {
    case Nil => returnST(())
    case ((i, v) :: ivs) => for {
      _ <- update(f, i, v)
      _ <- fill(f, ivs)
    } yield ()
  }

  /** Combine the given value with the value at the given index, using the given function. */
  def update[B](f: (A, B) => A, i: Int, v: B) = for {
    x <- read(i)
    _ <- write(i, f(x, v))
  } yield ()
}

/** 
 * Purely functional mutable state threads.
 * Based on JL and SPJ's paper "Lazy Functional State Threads"
 */
sealed trait ST[S, A] {
  private[effects] def apply(s: World[S]): (World[S], A)
  def flatMap[B](g: A => ST[S, B]): ST[S, B] =
    ST(s => apply(s) match { case (ns, a) => g(a)(ns) })
  def map[B](g: A => B): ST[S, B] =
    ST(s => apply(s) match { case (ns, a) => (ns, g(a)) })
}

object ST {
  def apply[S, A](f: World[S] => (World[S], A)) = new ST[S, A] {
    private[effects] def apply(s: World[S]) = f(s)
  }

}

