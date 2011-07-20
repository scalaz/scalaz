package scalaz
package iteratee

import IterateeT._
import Input._
import Identity._
import effect._

sealed trait EnumeratorT[X, E, F[_], A] {
  val runT: StepT[X, E, F, A] => IterateeT[X, E, F, A]

  def apply(s: StepT[X, E, F, A]): IterateeT[X, E, F, A] =
    runT(s)

}

object EnumeratorT extends EnumeratorTs {
  def apply[X, E, F[_], A](k: StepT[X, E, F, A] => IterateeT[X, E, F, A]): EnumeratorT[X, E, F, A] =
    enumeratorT[X, E, F, A](k)
}

trait EnumeratorTs {
  type Enumerator[X, E, A] =
  EnumeratorT[X, E, Identity, A]

  def enumeratorT[X, E, F[_], A](k: StepT[X, E, F, A] => IterateeT[X, E, F, A]): EnumeratorT[X, E, F, A] = new EnumeratorT[X, E, F, A] {
    val runT = k
  }

  def enumEofT[X, E, F[_]: Monad, A](e: (=> X) => IterateeT[X, E, F, A]): EnumeratorT[X, E, F, A] = {
    implicit val p = implicitly[Monad[F]].pointed
    implicit val b = implicitly[Monad[F]].bind
    enumeratorT(_.fold(
      cont = k =>
        k(eofInput) >>== (s =>
          s >- (
            sys.error("diverging iteratee")
          , enumEofT(e) runT s
          , enumEofT(e) runT s
          ))
    , done = (a, _) =>
        StepT.doneT[X, E, F, A](a, eofInput).pointI
    , err = e(_)
    ))
  }
}
