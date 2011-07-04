package scalaz
package iteratee

import IterateeT._
import EnumerateeT._
import Input._

sealed trait EnumerableT[X, E, F[_], A] {
  val apply: X => EnumerateeT[E, F, A]

  import EnumerableT._

  def contramap[Y](f: Y => X): EnumerableT[Y, E, F, A] =
    enumerableT(apply compose f)

}

object EnumerableT extends EnumerableTs

trait EnumerableTs {
  type Enumerable[X, E, A] =
  EnumerableT[X, E, Identity, A]

  def enumerableT[X, E, F[_], A](a: X => EnumerateeT[E, F, A]): EnumerableT[X, E, F, A] =
    new EnumerableT[X, E, F, A] {
      val apply = a
    }

  def enumerable[X, E, A](z: X => Enumeratee[E, A]): Enumerable[X, E, A] =
    enumerable(z)

  implicit def StreamEnumerable[E, A]: Enumerable[Stream[E], E, A] = new Enumerable[Stream[E], E, A] {
    val apply: Stream[E] => Enumeratee[E, A] =
      (x: Stream[E]) =>
        enumeratee((i: E >@> A) => x match {
          case Stream() => i
          case x #:: xs => i.fold(done = (_, _) => i, cont = k => apply(xs) enumerate k(elInput(x)) value)
        })
  }

}
