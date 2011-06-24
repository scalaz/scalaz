package scalaz
package iteratee

import Input._
import Identity._

sealed trait IterateeT[E, F[_], A] {

  import IterateeT._
  import EnumeratorT._

  def *->* : (({type λ[α] = IterateeT[E, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = IterateeT[E, F, α]})#λ, A](this)

  def *->*->* : *->*->*[E, ({type λ[α, β] = IterateeT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![E, ({type λ[α, β] = IterateeT[α, F, β]})#λ, A](this)

  def foldT[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => IterateeTM[E, F, A]) => Z): Z

  def fold[Z](done: (=> A, Input[E]) => Z, cont: (Input[E] => Iteratee[E, A]) => Z)(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): Z =
    foldT(
      (a, i) => done(a, i)
      , k => cont(g => i(k(g)).value)
    )

  def runT(implicit ftr: PointedFunctor[F]): F[A] =
    foldT(
      done = (x, _) => ftr.point(x)
      , cont = k =>
        ftr.fmap((d: IterateeT[E, F, A]) => d.foldT(
          done = (xx, _) => xx
          , cont = _ => sys.error("Diverging iteratee")
        ))(k(eofInput[E]))
    )

  def run(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): A =
    fold(
      done = (x, _) => x
      , cont = k =>
        k(eofInput[E]).fold(
          done = (xx, _) => xx
          , cont = _ => sys.error("Diverging iteratee")
        )
    )

  def map[B](f: A => B)(implicit ftr: Functor[F]): IterateeT[E, F, B] =
    foldT(
      (a, i) => doneT(f(a), i)
      , k => continueT(i => ftr.fmap((z: IterateeT[E, F, A]) => z map f)(k(i)))
    )

  def flatMap[B](f: A => IterateeT[E, F, B])(implicit m: Monad[F]): IterateeT[E, F, B] =
    foldT(
        (a, i) => f(a)
      , k => continueT(s =>
          m.bd((_: IterateeT[E, F, A]).foldT(
            done = (aa, ii) => f(aa).foldT(
              done = (aaa, iii) => m.point(doneT[F](aaa, iii))
            , cont = kk => kk(ii)
            )
          , cont = kk => m.point(continueT(kk) flatMap f)
          ))(k(s)))
    )

  def ifDoneElseCont[Z](done: => Z, cont: => Z): Z =
    foldT((_, _) => done, _ => cont)

  def contOrT(d: => Input[E] => IterateeTM[E, F, A]): Input[E] => IterateeTM[E, F, A] =
    foldT((_, _) => d, z => z)

  def contOr(d: => Input[E] => Iteratee[E, A])(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): Input[E] => Iteratee[E, A] =
    fold((_, _) => d, z => z)

  def inputOr(d: => Input[E])(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): Input[E] =
    fold((_, i) => i, _ => d)

  def liftIter(implicit s: Semigroup[E], p: Pointed[F]): EnumeratorT[E, F, A] =
    foldT(
      done = (a, i) => enumeratorT[E, F, A](i.el.fold(
        none = _ => p.point(doneT(a, i))
        , some = e => j => j.el.fold(
          none = p.point(doneT(a, j))
          , some = ee => p.point(doneT(a, elInput(s.append(e, ee))))
        )
      ))
      , cont = k => enumeratorT(k)
    )

  /** An iteratee that consumes the head of the input **/
  def head[E] : Iteratee[E, Option[E]] = {
    def step(s: Input[E]): Iteratee[E, Option[E]] =
      s(el = e => done(Some(e), emptyInput[E]),
        empty = continue(step),
        eof = done(None, eofInput[E]))
    continue(step)
  }

  /** An iteratee that returns the first element of the input **/
  def peek[E] : Iteratee[E, Option[E]] = {
    def step(s: Input[E]): Iteratee[E, Option[E]]
      = s(el = e => done(Some(e), s),
          empty = continue(step),
          eof = done(None, eofInput[E]))
    continue(step)
  }

  /** Peeks and returns either a Done iteratee with the given value or runs the given function with the peeked value **/
  def peekDoneOr[A, B](b: => B, f: A => Iteratee[A, B]): Iteratee[A, B] =
    peek[A] flatMap  {
      case None    => doneT(b, eofInput)
      case Some(a) => f(a)
    }

  /** An iteratee that skips the first n elements of the input **/
  def drop[E](n: Int): Iteratee[E, Unit] = {
    def step(s: Input[E]): Iteratee[E, Unit] =
      s(el = _ => drop(n - 1),
        empty = continue(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else continue(step)
  }

  /** An iteratee that counts and consumes the elements of the input **/
  def length[E] : Iteratee[E, Int] = {
    def step(acc: Int)(s: Input[E]): Iteratee[E, Int] =
      s(el = _ => continue(step(acc + 1)),
        empty = continue(step(acc)),
        eof = done(acc, eofInput[E]))
    continue(step(0))
  }

  /**
   * Takes while the given predicate holds, appending with the given monoid.
   */
  def takeWhile[A, F[_]](pred: A => Boolean)(implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[A, F[A]] = {
    def peekStepDoneOr(z: F[A]) = peekDoneOr(z, step(z, _: A))

    def step(acc: F[A], a: A): Iteratee[A, F[A]] =
      if (pred(a))
        drop(1) flatMap (_ => peekStepDoneOr(mon.append(acc, pt.point(a))))
      else
        done(acc, eofInput)

    peekStepDoneOr(mon.z)
  }

  /**
   * Produces chunked output split by the given predicate.
   */
  def groupBy[A, F[_]](pred: (A, A) => Boolean)(implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[A, F[A]] = {
    peek flatMap {
      case None    => done(mon.z, emptyInput[A])
      case Some(h) => takeWhile(pred(_, h))
    }
  }

  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeat[E, A, F[_]](iter: Iteratee[E,A])(implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[E, F[A]] = {
    def step(acc: F[A])(s: Input[E]): Iteratee[E, F[A]] =
      s(el = e => iter.fold(
          (a, _) => continue(step(mon.append(acc, pt.point(a)))),
          k => k(elInput(e)).fold(
            (a, _) => continue(step(mon.append(acc, pt.point(a)))),
            (k2) => continue(step(acc))
          )),
        empty = continue(step(acc)),
        eof = done(acc, eofInput))
    continue(step(mon.z))
  }

  /**
   * Iteratee that collects all inputs with the given monoid.
   */
  def collect[A, F[_]](implicit mon: Monoid[F[A]], pt: Pointed[F]): Iteratee[A, F[A]] = {
    def step(acc: F[A])(s: Input[A]): Iteratee[A, F[A]] =
        s(el = e => continue(step(mon.append(acc, pt.point(e)))),
          empty = continue(step(acc)),
          eof = done(acc, eofInput))
    continue(step(mon.z))
  }

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for F[_] with efficient cons, i.e. List.
   */
  def reversed[A, F[_]](implicit r: Reducer[A, F[A]]): Iteratee[A, F[A]] = {
    def step(acc: F[A])(s: Input[A]): Iteratee[A, F[A]] =
        s(el = e => continue(step(r.cons(e, acc))),
          empty = continue(step(acc)),
          eof = done(acc, eofInput))
    continue(step(r.monoid.z))
  }

}

object IterateeT extends IterateeTs {
  def apply[E, F[_], A](f: Input[E] => IterateeTM[E, F, A]): IterateeT[E, F, A] =
    continueT(f)
}

trait IterateeTs {
  type Iteratee[E, A] =
  IterateeT[E, Identity, A]

  type IterateeTM[E, F[_], A] =
  F[IterateeT[E, F, A]]

  type >@>[E, A] =
    Iteratee[E, A]

  sealed trait DoneT[F[_]] {
    def apply[E, A](a: => A, i: => Input[E]): IterateeT[E, F, A]
  }

  def doneT[F[_]]: DoneT[F] = new DoneT[F] {
    def apply[E, A](a: => A, i: => Input[E]): IterateeT[E, F, A] = new IterateeT[E, F, A] {
      def foldT[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => IterateeTM[E, F, A]) => Z) =
        done(a, i)
    }
  }

  def done[E, A](a: => A, i: => Input[E]): Iteratee[E, A] =
    doneT[Identity](a, i)

  def continueT[E, F[_], A](f: Input[E] => IterateeTM[E, F, A]): IterateeT[E, F, A] = new IterateeT[E, F, A] {
    def foldT[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => IterateeTM[E, F, A]) => Z) =
      cont(f)
  }

  def continue[E, A](f: Input[E] => Iteratee[E, A]): Iteratee[E, A] =
    continueT(i => id(f(i)))

  implicit def IterateeTMonadTrans[E]: MonadTrans[({type λ[α[_], β] = IterateeT[E, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = IterateeT[E, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): IterateeT[E, G, A] =
      continueT(i =>
        implicitly[Monad[G]].fmap((aa: A) => doneT[G](aa, emptyInput[E]))(a))
  }

}