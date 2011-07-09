package scalaz
package iteratee

import Iteratee._

sealed trait EnumerateeT[I, O, F[_], A] {
  val apply: IterateeT[I, F, A] => IterateeT[O, F, IterateeT[I, F, A]]
}

object EnumerateeT extends EnumerateeTs

trait EnumerateeTs {
  def enumerateeT[I, O, F[_], A](k: IterateeT[I, F, A] => IterateeT[O, F, IterateeT[I, F, A]]): EnumerateeT[I, O, F, A] =
    new EnumerateeT[I, O, F, A] {
      val apply = k
    }
  
  def continueOrDone[O, I, F[_], A](f: (Input[I] => IterT[I, F, A]) => IterateeT[O, F, IterateeT[I, F, A]])(inner: IterateeT[I, F, A]) =
    inner.foldT(
      done = (x, _) => doneT[F](doneT[F](x, emptyInput[I]), emptyInput[O])
      , cont = k => f(k)
    )

  /**
   * Takes while the given predicate holds.
   */
  def takeWhile[E, F[_], A](p: E => Boolean)(implicit ftr: PointedFunctor[F]): EnumerateeT[E, E, F, A] = {
    def step(k: Input[E] => IterT[E, F, A]): Input[E] => IterT[E, F, IterateeT[E, F, A]] = { in => in(
      empty = ftr.point(continueT(step(k)))
      , el = el =>
        if (p(el)) (ftr.fmap(continueOrDone[E, E, F, A] { k2 => continueT(step(k2)) }))(k(in))
        else (ftr.fmap(doneT[F](_:IterateeT[E, F, A], in)))(k(eofInput))
      , eof = (ftr.fmap(doneT[F](_:IterateeT[E, F, A], in)))(k(in))
    )}
    
    enumerateeT(continueOrDone { k => continueT(step(k)) })
  }

  /**
   * Produces chunked output split by the given predicate.
   */
//  def groupBy[I, O, F[_], A](pred: (A, A) => Boolean): EnumerateeT[I, O, F, A] = {
//    peek flatMap {
//      case None => done(mon.z, emptyInput[A])
//      case Some(h) => takeWhile(pred(_, h))
//    }
//  }
}
