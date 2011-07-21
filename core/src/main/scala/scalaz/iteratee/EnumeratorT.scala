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
        StepT.sdone[X, E, F, A](a, eofInput).pointI
    , err = e(_)
    ))
  }

  def checkCont0[X, E, F[_], A](z: EnumeratorT[X, E, F, A] => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
      s =>
        s.mapContOr(
          k => z(enumeratorT(step))(k)
        , s.pointI
        )
    }
    enumeratorT(step)
  }

  def checkCont1[S, X, E, F[_], A](z: (S => EnumeratorT[X, E, F, A]) => S => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A], t: S)(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: S => StepT[X, E, F, A] => IterateeT[X, E, F, A] = {
      o => s =>
        s.mapContOr(
          k => z(w => enumeratorT(step(w)))(o)(k)
        , s.pointI
        )
    }
    enumeratorT(step(t))
  }

  def iterate[X, E, F[_]: Monad, A](f: E => E, e: E): EnumeratorT[X, E, F, A] = {
    implicit val bd = implicitly[Monad[F]].bind
    implicit val pt = implicitly[Monad[F]].pointed
    checkCont1[E, X, E, F, A](s => t => k => k(elInput(e)) >>== s(f(t)).runT, e)
  }

  def repeat[X, E, F[_]: Monad, A](e: E): EnumeratorT[X, E, F, A] = {
    implicit val bd = implicitly[Monad[F]].bind
    implicit val pt = implicitly[Monad[F]].pointed
    checkCont0[X, E, F, A](s => k => k(elInput(e)) >>== s.runT)
  }
}
