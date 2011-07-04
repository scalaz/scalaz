package scalaz
package iteratee

import IterateeT._
import Identity._

sealed trait EnumerateeT[E, F[_], A] {
  val enumerate: Iteratee[E, A] => IterT[E, F, A]
}

object EnumerateeT extends EnumerateeTs {
  def apply[E, F[_], A](k: Iteratee[E, A] => IterT[E, F, A]): EnumerateeT[E, F, A] =
    enumerateeT[E, F, A](k)
}

trait EnumerateeTs {
  type Enumeratee[E, A] =
  EnumerateeT[E, Identity, A]

  def enumerateeT[E, F[_], A](k: Iteratee[E, A] => IterT[E, F, A]): EnumerateeT[E, F, A] = new EnumerateeT[E, F, A] {
    val enumerate = k
  }

  def enumeratee[E, A](k: Iteratee[E, A] => Iteratee[E, A]): Enumeratee[E, A] =
    enumerateeT[E, Identity, A](i => id(k(i)))
}
