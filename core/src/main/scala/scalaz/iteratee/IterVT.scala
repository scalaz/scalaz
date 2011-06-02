package scalaz
package iteratee

import Input._
import Ident._

sealed trait IterVT[E, F[_], A] {

  import IterVT._
  import IterateeT._

  def *->* : (({type λ[α] = IterVT[E, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = IterVT[E, F, α]})#λ, A](this)

  def *->*->* : *->*->*[E, ({type λ[α, β] = IterVT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![E, ({type λ[α, β] = IterVT[α, F, β]})#λ, A](this)

  def foldT[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => FIterVT[E, F, A]) => Z): Z

  def fold[Z](done: (=> A, Input[E]) => Z, cont: (Input[E] => IterV[E, A]) => Z)(implicit i: F[IterVT[E, F, A]] =:= Ident[IterVT[E, Ident, A]]): Z =
    foldT(
      (a, i) => done(a, i)
      , k => cont(g => i(k(g)).value)
    )

  def runT(implicit ftr: PointedFunctor[F]): F[A] =
    foldT(
      done = (x, _) => ftr.point(x)
      , cont = k =>
        ftr.fmap((d: IterVT[E, F, A]) => d.foldT(
          done = (xx, _) => xx
          , cont = _ => sys.error("Diverging iteratee")
        ))(k(eofInput[E]))
    )

  def run(implicit i: F[IterVT[E, F, A]] =:= Ident[IterVT[E, Ident, A]]): A =
    fold(
      done = (x, _) => x
      , cont = k =>
        k(eofInput[E]).fold(
          done = (xx, _) => xx
          , cont = _ => sys.error("Diverging iteratee")
        )
    )

  def map[B](f: A => B)(implicit ftr: Functor[F]): IterVT[E, F, B] =
    foldT(
      (a, i) => doneT(f(a), i)
      , k => continueT(i => ftr.fmap((z: IterVT[E, F, A]) => z map f)(k(i)))
    )

  def ifDoneElseCont[Z](done: => Z, cont: => Z): Z =
    foldT((_, _) => done, _ => cont)

  def contOrT(d: => Input[E] => FIterVT[E, F, A]): Input[E] => FIterVT[E, F, A] =
    foldT((_, _) => d, z => z)

  def contOr(d: => Input[E] => IterV[E, A])(implicit i: F[IterVT[E, F, A]] =:= Ident[IterVT[E, Ident, A]]): Input[E] => IterV[E, A] =
    fold((_, _) => d, z => z)

  def inputOr(d: => Input[E])(implicit i: F[IterVT[E, F, A]] =:= Ident[IterVT[E, Ident, A]]): Input[E] =
    fold((_, i) => i, _ => d)

  def liftIter(implicit s: Semigroup[E], p: Pointed[F]): IterateeT[E, F, A] =
    foldT(
      done = (a, i) => iterateeT[E, F, A](i.el.fold(
        none = _ => p.point(doneT(a, i))
        , some = e => j => j.el.fold(
          none = p.point(doneT(a, j))
          , some = ee => p.point(doneT(a, elInput(s.append(e, ee))))
        )
      ))
      , cont = k => iterateeT(k)
    )
}

object IterVT extends IterVTs {
  def apply[E, F[_], A](f: Input[E] => FIterVT[E, F, A]): IterVT[E, F, A] =
    continueT(f)
}

trait IterVTs {
  type IterV[E, A] =
  IterVT[E, Ident, A]

  type FIterVT[E, F[_], A] =
  F[IterVT[E, F, A]]

  def doneT[E, F[_], A](a: => A, i: => Input[E]): IterVT[E, F, A] = new IterVT[E, F, A] {
    def foldT[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => FIterVT[E, F, A]) => Z) =
      done(a, i)
  }

  def done[E, A](a: => A, i: => Input[E]): IterV[E, A] =
    doneT[E, Ident, A](a, i)

  def continueT[E, F[_], A](f: Input[E] => FIterVT[E, F, A]): IterVT[E, F, A] = new IterVT[E, F, A] {
    def foldT[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => FIterVT[E, F, A]) => Z) =
      cont(f)
  }

  def continue[E, A](f: Input[E] => IterV[E, A]): IterV[E, A] =
    continueT(i => ident(f(i)))
}