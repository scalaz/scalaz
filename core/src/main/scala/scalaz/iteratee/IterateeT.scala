package scalaz
package iteratee

import IterVT._
import Input._
import Ident._

sealed trait IterateeT[E, F[_], A] {
  val iterate: Input[E] => FIterVT[E, F, A]

  def *->* : (({type λ[α] = IterateeT[E, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = IterateeT[E, F, α]})#λ, A](this)

  def *->*->* : *->*->*[E, ({type λ[α, β] = IterateeT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![E, ({type λ[α, β] = IterateeT[α, F, β]})#λ, A](this)

  import IterateeT._

  def map[B](f: A => B)(implicit ftr: Functor[F]): IterateeT[E, F, B] =
    iterateeT[E, F, B](i =>
      ftr.fmap((d: IterVT[E, F, A]) => d map f)(iterate(i)))

  def flatMap[B](f: A => IterateeT[E, F, B])(implicit mnd: Monad[F]): IterateeT[E, F, B] =
    iterateeT[E, F, B](i =>
      mnd.bd((d: IterVT[E, F, A]) =>
        d.foldT(
          (a, i) => f(a) iterate i
          , k => mnd.point(continueT(iterateeT[E, F, A](k) flatMap f iterate (_: Input[E])))
        ))(iterate(i)))

  def continue: IterVT[E, F, A] =
    continueT(iterate)

  def runT(implicit mnd: Monad[F]): F[A] = {
    implicit val p = mnd.pointedFunctor
    mnd.bd((r: IterVT[E, F, A]) => r.runT)(iterate(eofInput[E]))
  }

  def run(implicit i: F[IterVT[E, F, A]] =:= Ident[IterVT[E, Ident, A]]): A =
    iterate(eofInput[E]).value.run
}

object IterateeT extends IterateeTs {
  def apply[E, F[_], A](k: Input[E] => FIterVT[E, F, A]): IterateeT[E, F, A] =
    iterateeT[E, F, A](k)
}

trait IterateeTs {
  type Iteratee[E, A] =
  IterateeT[E, Ident, A]

  def iterateeT[E, F[_], A](k: Input[E] => FIterVT[E, F, A]): IterateeT[E, F, A] = new IterateeT[E, F, A] {
    val iterate = k
  }

  def iteratee[E, A](k: Input[E] => IterV[E, A]): Iteratee[E, A] =
    iterateeT[E, Ident, A](i => ident(k(i)))
}
