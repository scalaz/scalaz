package scalaz
package iteratee

import Input._
import Identity._

sealed trait IterateeT[E, F[_], A] {

  import IterateeT._
  import EnumeratorT._
  import =~~=._

  def *->* : (({type λ[α] = IterateeT[E, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = IterateeT[E, F, α]})#λ, A](this)

  def *->*->* : *->*->*[E, ({type λ[α, β] = IterateeT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![E, ({type λ[α, β] = IterateeT[α, F, β]})#λ, A](this)

  def foldT[Z](done: (=> A, => Input[E]) => F[Z], cont: (Input[E] => IterateeT[E, F, A]) => F[Z]): F[Z]

  def iteratee(implicit iso: F =~~= Identity): (E >@> A) =
    foldT[E >@> A](
      (a, i) => <=~~[F, E >@> A](done(a, i))
    , k => <=~~[F, E >@> A](continue[E, A](k(_).iteratee)))

  def fold[Z](done: (=> A, => Input[E]) => Z, cont: (Input[E] => Iteratee[E, A]) => Z)(implicit iso: F =~~= Identity): Z =
    iteratee.foldT[Z](
      (a, i) => id(done(a, i))
    , k => id(cont(k))
    )

  def runT(implicit p: Pointed[F]): F[A] =
    foldT(
      done = (x, _) => p.point(x)
      , cont = k =>
          k(eofInput[E]).foldT(
            done = (xx, _) => p.point(xx)
          , cont = _ => sys.error("Diverging iteratee")
          )
    )

  def run(implicit iso: F =~~= Identity): A =
    runT


  def map[B](f: A => B)(implicit ftr: Functor[F]): IterateeT[E, F, B] =
    new IterateeT[E, F, B] {
      def foldT[Z](done: (=> B, => Input[E]) => F[Z], cont: (Input[E] => IterateeT[E, F, B]) => F[Z]) = {
        IterateeT.this.foldT(
          done = (a, i) => done(f(a), i)
        , cont = k => cont(k(_) map f)
        )
      }
    }

  /*
    foldT(
      (a, _) => f(a)
      , k => continueT(s =>
        m.bd((_: IterateeT[E, F, A]).foldT(
          done = (aa, ii) => f(aa).foldT(
            done = (aaa, iii) => m.point(doneT[F](aaa, iii))
            , cont = kk => kk(ii)
          )
          , cont = kk => m.point(continueT(kk) flatMap f)
        ))(k(s)))
    )
    */
     /*
  def enumerateT(e: EnumeratorT[E, F, A]): IterT[E, F, A] =
    e enumerateT this

  def enumerate(e: Enumerator[E, A])(implicit x: IterateeT[E, F, A] =:= IterateeT[E, Identity, A], y: IterT[E, Identity, A] =:= IterT[E, F, A]): Iteratee[E, A] =
    e enumerate this

  def enumerateUp[G[_]](e: EnumeratorT[E, G, A])(implicit p: Pointed[G], t: CoPointedFunctor[F]): IterT[E, G, A] =
    e enumerateUp this

  def mapI[G[_]](f: F ~> G)(implicit ftr: Functor[F]): IterateeT[E, G, A] =
    foldT(
      done = IterateeT.doneT(_, _)
      , cont = k => Iteratee.continueT(i => f(ftr.fmap((x: IterateeT[E, F, A]) => x mapI f)(k(i))))
    )

  def up[G[_]](implicit p: Pointed[G], f: CoPointedFunctor[F]): IterateeT[E, G, A] = {
    implicit val ftr = f.functor
    mapI(new (F ~> G) {
      def apply[A](a: F[A]) = implicitly[Pointed[G]].point(implicitly[CoPointedFunctor[F]].coPoint(a))
    })
  }

  def ifDoneElseCont[Z](done: => Z, cont: => Z): Z =
    foldT((_, _) => done, _ => cont)

  def contOrT(d: => Input[E] => IterT[E, F, A]): Input[E] => IterT[E, F, A] =
    foldT((_, _) => d, z => z)

  def contOr(d: => Input[E] => Iteratee[E, A])(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): Input[E] => Iteratee[E, A] =
    fold((_, _) => d, z => z)

  def inputOr(d: => Input[E])(implicit i: F[IterateeT[E, F, A]] =:= Identity[IterateeT[E, Identity, A]]): Input[E] =
    fold((_, i) => i, _ => d)
    */

}

object IterateeT extends IterateeTs {
  /*
  def apply[E, F[_], A](f: Input[E] => IterT[E, F, A]): IterateeT[E, F, A] =
    continueT(f)
    */
}

trait IterateeTs {
  type Iteratee[E, A] =
  IterateeT[E, Identity, A]

  type >@>[E, A] =
  Iteratee[E, A]

  import =~~=._

  sealed trait DoneT[F[_]] {
    def apply[E, A](a: => A, i: => Input[E]): IterateeT[E, F, A]
  }

  def doneT[F[_]]: DoneT[F] = new DoneT[F] {
    def apply[E, A](a: => A, i: => Input[E]): IterateeT[E, F, A] = new IterateeT[E, F, A] {
      def foldT[Z](done: (=> A, => Input[E]) => F[Z], cont: (Input[E] => IterateeT[E, F, A]) => F[Z]) =
        done(a, i)
    }
  }

  def done[E, A](a: => A, i: => Input[E]): (E >@> A) =
    doneT[Identity](a, i)

  def continueT[E, F[_], A](f: Input[E] => IterateeT[E, F, A]): IterateeT[E, F, A] = new IterateeT[E, F, A] {
    def foldT[Z](done: (=> A, => Input[E]) => F[Z], cont: (Input[E] => IterateeT[E, F, A]) => F[Z]) =
      cont(f)
  }

  def continue[E, A](f: Input[E] => E >@> A): (E >@> A) =
    continueT(i => f(i))

  /**An iteratee that consumes the head of the input **/
  def head[E]: (E >@> Option[E]) = {
    def step(s: Input[E]): (E >@> Option[E]) =
      s(el = e => done(Some(e), emptyInput[E]),
        empty = continue(step),
        eof = done(None, eofInput[E]))
    continue(step)
  }

  /**An iteratee that returns the first element of the input **/
  def peek[E]: (E >@> Option[E]) = {
    def step(s: Input[E]): (E >@> Option[E])
    = s(el = e => done(Some(e), s),
      empty = continue(step),
      eof = done(None, eofInput[E]))
    continue(step)
  }
             /* todo
  /**Peeks and returns either a Done iteratee with the given value or runs the given function with the peeked value **/
  def peekDoneOr[A, B](b: => B, f: A => A >@> B): (A >@> B) =
    peek[A] flatMap {
      case None => doneT(b, eofInput)
      case Some(a) => f(a)
    }
    */

  /**An iteratee that skips the first n elements of the input **/
  def drop[E](n: Int): (E >@> Unit) = {
    def step(s: Input[E]): (E >@> Unit) =
      s(el = _ => drop(n - 1),
        empty = continue(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else continue(step)
  }

  /**An iteratee that counts and consumes the elements of the input **/
  def length[E]: (E >@> Int) = {
    def step(acc: Int)(s: Input[E]): (E >@> Int) =
      s(el = _ => continue(step(acc + 1)),
        empty = continue(step(acc)),
        eof = done(acc, eofInput[E]))
    continue(step(0))
  }

  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeat[E, A, F[_]](iter: E >@> A)(implicit mon: Monoid[F[A]], pt: Pointed[F]): (E >@> F[A]) = {
    def step(acc: F[A])(s: Input[E]): (E >@> F[A]) =
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
  def collect[A, F[_]](implicit mon: Monoid[F[A]], pt: Pointed[F]): (A >@> F[A]) = {
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
  def reversed[A, F[_]](implicit r: Reducer[A, F[A]]): (A >@> F[A]) = {
    def step(acc: F[A])(s: Input[A]): Iteratee[A, F[A]] =
      s(el = e => continue(step(r.cons(e, acc))),
        empty = continue(step(acc)),
        eof = done(acc, eofInput))
    continue(step(r.monoid.z))
  }
        /*
  implicit def IterateeTMonadTrans[E]: MonadTrans[({type λ[α[_], β] = IterateeT[E, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = IterateeT[E, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): IterateeT[E, G, A] =
      continueT(i =>
        implicitly[Monad[G]].fmap((aa: A) => doneT[G](aa, emptyInput[E]))(a))
  }

  implicit def IterateeTPointed[X, F[_] : Pointed]: Pointed[({type λ[α] = IterateeT[X, F, α]})#λ] =
    new Pointed[({type λ[α] = IterateeT[X, F, α]})#λ] {
      def point[A](a: => A) =
        doneT(a, emptyInput)
    }

  implicit def IterateeTFunctor[X, F[_] : Functor]: Functor[({type λ[α] = IterateeT[X, F, α]})#λ] =
    new Functor[({type λ[α] = IterateeT[X, F, α]})#λ] {
      def fmap[A, B](f: A => B) =
        _ map f
    }

  implicit def IterateeTApplic[X, F[_] : Monad]: Applic[({type λ[α] = IterateeT[X, F, α]})#λ] =
    new Applic[({type λ[α] = IterateeT[X, F, α]})#λ] {
      implicit val ftr = implicitly[Monad[F]].functor

      def applic[A, B](f: IterateeT[X, F, A => B]) =
        a =>
          for {
            ff <- f
            aa <- a
          } yield ff(aa)
    }

  implicit def IterateeTBind[X, F[_] : Monad]: Bind[({type λ[α] = IterateeT[X, F, α]})#λ] =
    new Bind[({type λ[α] = IterateeT[X, F, α]})#λ] {
      def bind[A, B](f: A => IterateeT[X, F, B]) =
        _ flatMap f
    }

  implicit def IterateeTApplicFunctor[X, F[_] : Monad]: ApplicFunctor[({type λ[α] = IterateeT[X, F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    ApplicFunctor.applicFunctor[({type λ[α] = IterateeT[X, F, α]})#λ]
  }

  implicit def IterateeTPointedFunctor[X, F[_] : Monad]: PointedFunctor[({type λ[α] = IterateeT[X, F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    implicit val p = implicitly[Monad[F]].pointed
    PointedFunctor.pointedFunctor[({type λ[α] = IterateeT[X, F, α]})#λ]
  }

  implicit def IterateeTApplicative[X, F[_] : Monad]: Applicative[({type λ[α] = IterateeT[X, F, α]})#λ] = {
    implicit val ap = implicitly[Monad[F]].applic
    implicit val p = implicitly[Monad[F]].pointedFunctor
    Applicative.applicative[({type λ[α] = IterateeT[X, F, α]})#λ]
  }

  implicit def IterateeTMonad[X, F[_] : Monad]: Monad[({type λ[α] = IterateeT[X, F, α]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val p = implicitly[Monad[F]].pointed
    Monad.monadBP[({type λ[α] = IterateeT[X, F, α]})#λ]
  }
  */

}