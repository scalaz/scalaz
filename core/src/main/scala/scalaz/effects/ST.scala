package scalaz
package effects

import Scalaz._
import Free._


private[effects] case class World[A]()
sealed trait RealWorld

/** Mutable variable in state thread S containing a value of type A. */
class STRef[S, A](a: A) {
  private var value: A = a

  /** Reads the value pointed at by this reference. */
  def read: ST[S, A] = returnST(value)

  /** Modifies the value at this reference with the given function. */
  def mod[B](f: A => A): ST[S, STRef[S, A]] = ST((s: World[S]) =>
    {value = f(value); Return((s, this))})

  /** Associates this reference with the given value. */
  def write(a: => A): ST[S, STRef[S, A]] = ST((s: World[S]) =>
    {value = a; Return((s, this))})

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
  def write(i: Int, a: A): ST[S, STArray[S, A]] =
    ST(s => {value(i) = a; Return((s, this))})

  /** Turns a mutable array into an immutable one which is safe to return. */
  def freeze: ST[S, ImmutableArray[A]] =
    ST(s => Return((s, ImmutableArray.fromArray(value))))

  /** Fill this array from the given association list. */
  def fill[F[_]: Foldable, B](f: (A, B) => A, xs: F[(Int, B)]): ST[S, Unit] = 
    ST(s => {
        xs.foldl(())((_, b) => {
          val x = b._1
          value(x) = f(value(x), b._2)
        })
        Return((s, ()))})

  /** Combine the given value with the value at the given index, using the given function. */
  def update[B](f: (A, B) => A, i: Int, v: B): ST[S, Unit] =
    ST(s => Return({value(i) = f(value(i), v); (s, ())}))

  /** Fill this array from the given immutable array. */
  def copyFromArray(as: ImmutableArray[A]): ST[S, Unit] =
    ST(s => Return((s, as.copyToArray(value, 0, size))))
}

/** 
 * Purely functional mutable state threads.
 * Based on JL and SPJ's paper "Lazy Functional State Threads"
 */
sealed trait ST[S, A] {
  private[effects] def apply(s: World[S]): Trampoline[(World[S], A)]
  def flatMap[B](g: A => ST[S, B]): ST[S, B] =
    ST(s => Suspend(() => apply(s) flatMap { case (ns, a) => Suspend(() => g(a)(ns)) }))
  def map[B](g: A => B): ST[S, B] =
    ST(s => Suspend(() => apply(s) flatMap { case (ns, a) => Suspend(() => Return((ns, g(a)))) }))
}

object ST {
  def apply[S, A](f: World[S] => Trampoline[(World[S], A)]) = new ST[S, A] {
    private[effects] def apply(s: World[S]) = f(s)
  }
}
