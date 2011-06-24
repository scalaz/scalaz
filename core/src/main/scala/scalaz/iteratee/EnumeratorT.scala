package scalaz
package iteratee

import IterateeT._
import Input._
import Identity._

sealed trait EnumeratorT[E, F[_], A] {
  val enumerate: Input[E] => IterateeTM[E, F, A]

  def *->* : (({type λ[α] = EnumeratorT[E, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = EnumeratorT[E, F, α]})#λ, A](this)

  def *->*->* : *->*->*[E, ({type λ[α, β] = EnumeratorT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![E, ({type λ[α, β] = EnumeratorT[α, F, β]})#λ, A](this)

  import EnumeratorT._

  def map[B](f: A => B)(implicit ftr: Functor[F]): EnumeratorT[E, F, B] =
    enumeratorT[E, F, B](i =>
      ftr.fmap((d: IterateeT[E, F, A]) => d map f)(enumerate(i)))

  def flatMap[B](f: A => EnumeratorT[E, F, B])(implicit mnd: Monad[F]): EnumeratorT[E, F, B] =
    enumeratorT[E, F, B](i =>
      mnd.bd((d: IterateeT[E, F, A]) =>
        d.foldT(
          (a, i) => f(a) enumerate i
          , k => mnd.point(continueT(enumeratorT[E, F, A](k) flatMap f enumerate (_: Input[E])))
        ))(enumerate(i)))

  def continue: IterateeT[E, F, A] =
    continueT(enumerate)

  def runT(implicit mnd: Monad[F]): F[A] = {
    implicit val p = mnd.pointedFunctor
    mnd.bd((r: IterateeT[E, F, A]) => r.runT)(enumerate(eofInput[E]))
  }

  def run(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): A =
    enumerate(eofInput[E]).value.run
}

object EnumeratorT extends EnumeratorTs {
  def apply[E, F[_], A](k: Input[E] => IterateeTM[E, F, A]): EnumeratorT[E, F, A] =
    enumeratorT[E, F, A](k)
}

trait EnumeratorTs {
  type Enumerator[E, A] =
  EnumeratorT[E, Identity, A]

  def enumeratorT[E, F[_], A](k: Input[E] => IterateeTM[E, F, A]): EnumeratorT[E, F, A] = new EnumeratorT[E, F, A] {
    val enumerate = k
  }

  def enumerator[E, A](k: Input[E] => Iteratee[E, A]): Enumerator[E, A] =
    enumeratorT[E, Identity, A](i => id(k(i)))
}
