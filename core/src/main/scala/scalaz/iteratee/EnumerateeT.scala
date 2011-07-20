package scalaz
package iteratee

import Iteratee._

sealed trait EnumerateeT[X, O, I, F[_], A] {
  val enumerateT: StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]]

  def apply(s: StepT[X, I, F, A]): IterateeT[X, O, F, StepT[X, I, F, A]] =
    enumerateT(s)
}

object EnumerateeT extends EnumerateeTs {
  def apply[X, O, I, F[_], A](k: StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]]): EnumerateeT[X, O, I, F, A] =
    enumerateeT(k)
}

trait EnumerateeTs {
  def enumerateeT[X, O, I, F[_], A](k: StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]]): EnumerateeT[X, O, I, F, A] =
    new EnumerateeT[X, O, I, F, A] {
      val enumerateT = k
    }
}
