package scalaz
package iteratee

import Input._
import Identity._
import EnumeratorT._
import StepT._

sealed trait IterateeT[X, E, F[_], A] {
  val value: F[StepT[X, E, F, A]]

  def *->* : (({type λ[α] = IterateeT[X, E, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = IterateeT[X, E, F, α]})#λ, A](this)

  def *->*->* : *->*->*[E, ({type λ[α, β] = IterateeT[X, α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![E, ({type λ[α, β] = IterateeT[X, α, F, β]})#λ, A](this)

  import IterateeT._
  import =~~=._

  def foldT[Z](
    cont: (Input[E] => IterateeT[X, E, F, A]) => F[Z]
  , done: (=> A, => Input[E]) => F[Z]
  , err: (=> X) => F[Z]
  )(implicit m: Bind[F]): F[Z] =
    m.bind((s: StepT[X, E, F, A]) => s(cont, done, err))(value)

  def fold[Z](
    cont: (Input[E] => IterateeT[X, E, F, A]) => Z
  , done: (=> A, => Input[E]) => Z
  , err: (=> X) => Z
  )(implicit i: F =~~= Identity): Z =
    foldT(k => <=~~[F, Z](cont(k)), (a, o) => <=~~[F, Z](done(a, o)), e => <=~~[F, Z](err(e)))

  def runT(e: (=> X) => F[A])(implicit m: Monad[F]): F[A] = {
    implicit val b = m.bind
    val lifte: (=> X) => IterateeT[X, E, F, A] = x => implicitly[MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ]].lift(e(x))
    m.bd((s: StepT[X, E, F, A]) => s.fold(
      cont = _ => sys.error("diverging iteratee")
    , done = (a, _) => m.point(a)
    , err = e
    ))(>>==(enumEofT(lifte).runT).value)
  }

  def run(e: (=> X) => A)(implicit i: F =~~= Identity): A =
    runT(x => <=~~[F, A](e(x)))

  def flatMap[B](f: A => IterateeT[X, E, F, B])(implicit m: Monad[F]): IterateeT[X, E, F, B] = {
    def through(x: IterateeT[X, E, F, A]): IterateeT[X, E, F, B] =
      iterateeT(
        m.bd((s: StepT[X, E, F, A]) => s.fold[F[StepT[X, E, F, B]]](
          cont = k => m.point(StepT.scont(u => through(k(u))))
        , done = (a, i) =>
            if(i.isEmpty)
              f(a).value
            else
              m.bd((ss: StepT[X, E, F, B]) => ss.fold(
                cont = kk => kk(i).value
              , done = (aa, _) => m.point(StepT.sdone[X, E, F, B](aa, i))
              , err = ee => m.point(StepT.serr[X, E, F, B](ee))
              ))(f(a).value)
        , err = e => m.point(StepT.serr(e))
        ))(x.value))
    through(this)
  }

  def map[B](f: A => B)(implicit m: Monad[F]): IterateeT[X, E, F, B] = {
    implicit val p = m.pointed
    flatMap(a => StepT.sdone[X, E, F, B](f(a), emptyInput).pointI)
  }

  def >>==[B, C](f: StepT[X, E, F, A] => IterateeT[X, B, F, C])(implicit m: Bind[F]): IterateeT[X, B, F, C] =
    iterateeT(m.bind((s: StepT[X, E, F, A]) => f(s).value)(value))

}

object IterateeT extends IterateeTs {
  def apply[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] =
    iterateeT(s)
}

trait IterateeTs {
  type Iteratee[X, E, A] =
  IterateeT[X, E, Identity, A]

  type >@>[E, A] =
  Iteratee[Unit, E, A]

  def iterateeT[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] = new IterateeT[X, E, F, A] {
    val value = s
  }

  def iteratee[X, E, A](s: Step[X, E, A]): Iteratee[X, E, A] =
    iterateeT(Identity.id(s))

  def cont[X, E, F[_]: Pointed, A](c: Input[E] => IterateeT[X, E, F, A]): IterateeT[X, E, F, A] =
    iterateeT(implicitly[Pointed[F]].point(StepT.scont(c)))

  def done[X, E, F[_]: Pointed, A](d: => A, r: => Input[E]): IterateeT[X, E, F, A] =
    iterateeT(implicitly[Pointed[F]].point(StepT.sdone(d, r)))

  def err[X, E, F[_]: Pointed, A](e: => X): IterateeT[X, E, F, A] =
    iterateeT(implicitly[Pointed[F]].point(StepT.serr(e)))

  implicit def IterateeTPointed[X, E, F[_] : Pointed]: Pointed[({type λ[α] = IterateeT[X, E, F, α]})#λ] =
    new Pointed[({type λ[α] = IterateeT[X, E, F, α]})#λ] {
      def point[A](a: => A) =
        StepT.sdone(a, emptyInput).pointI
    }

  implicit def IterateeTFunctor[X, E, F[_] : Monad]: Functor[({type λ[α] = IterateeT[X, E, F, α]})#λ] =
    new Functor[({type λ[α] = IterateeT[X, E, F, α]})#λ] {
      def fmap[A, B](f: A => B) =
        _ map f
    }

  implicit def IterateeTApplic[X, E, F[_] : Monad]: Applic[({type λ[α] = IterateeT[X, E, F, α]})#λ] =
    new Applic[({type λ[α] = IterateeT[X, E, F, α]})#λ] {
      implicit val ftr = implicitly[Monad[F]].functor

      def applic[A, B](f: IterateeT[X, E, F, A => B]) =
        a =>
          for {
            ff <- f
            aa <- a
          } yield ff(aa)
    }

  implicit def IterateeTBind[X, E, F[_] : Monad]: Bind[({type λ[α] = IterateeT[X, E, F, α]})#λ] =
    new Bind[({type λ[α] = IterateeT[X, E, F, α]})#λ] {
      def bind[A, B](f: A => IterateeT[X, E, F, B]) =
        _ flatMap f
    }

  implicit def IterateeTApplicFunctor[X, E, F[_] : Monad]: ApplicFunctor[({type λ[α] = IterateeT[X, E, F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    ApplicFunctor.applicFunctor[({type λ[α] = IterateeT[X, E, F, α]})#λ]
  }

  implicit def IterateeTBindFunctor[X, E, F[_] : Monad]: BindFunctor[({type λ[α] = IterateeT[X, E, F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    BindFunctor.bindFunctor[({type λ[α] = IterateeT[X, E, F, α]})#λ]
  }

  implicit def IterateeTPointedFunctor[X, E, F[_] : Monad]: PointedFunctor[({type λ[α] = IterateeT[X, E, F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    implicit val p = implicitly[Monad[F]].pointed
    PointedFunctor.pointedFunctor[({type λ[α] = IterateeT[X, E, F, α]})#λ]
  }

  implicit def IterateeTApplicative[X, E, F[_] : Monad]: Applicative[({type λ[α] = IterateeT[X, E, F, α]})#λ] = {
    implicit val ap = implicitly[Monad[F]].applic
    implicit val p = implicitly[Monad[F]].pointedFunctor
    Applicative.applicative[({type λ[α] = IterateeT[X, E, F, α]})#λ]
  }

  implicit def IterateeTMonad[X, E, F[_] : Monad]: Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val p = implicitly[Monad[F]].pointed
    Monad.monadBP[({type λ[α] = IterateeT[X, E, F, α]})#λ]
  }

  implicit def IterateeTMonadTrans[X, E]: MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): IterateeT[X, E, G, A] =
      iterateeT(implicitly[Monad[G]].fmap((x: A) => StepT.sdone[X, E, G, A](x, emptyInput))(a))
  }

  /**An iteratee that consumes the head of the input **/
  def head[E]: (E >@> Option[E]) = {
    def step(s: Input[E]): (E >@> Option[E]) =
      s(empty = cont(step)
      , el = e => done(Some(e), emptyInput[E])
      , eof = done(None, eofInput[E])
      )
    cont(step)
  }

  /**An iteratee that returns the first element of the input **/
  def peek[E]: (E >@> Option[E]) = {
    def step(s: Input[E]): (E >@> Option[E])
    = s(el = e => done(Some(e), s),
      empty = cont(step),
      eof = done(None, eofInput[E]))
    cont(step)
  }

  def peekDoneOr[A, B](b: => B, f: A => A >@> B): (A >@> B) =
    peek[A] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }

  /**An iteratee that skips the first n elements of the input **/
  def drop[E](n: Int): (E >@> Unit) = {
    def step(s: Input[E]): (E >@> Unit) =
      s(el = _ => drop(n - 1),
        empty = cont(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else cont(step)
  }

  /**An iteratee that counts and consumes the elements of the input **/
  def length[E]: (E >@> Int) = {
    def step(acc: Int)(s: Input[E]): (E >@> Int) =
      s(el = _ => cont(step(acc + 1)),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(0))
  }

  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeat[E, A, F[_]](iter: E >@> A)(implicit mon: Monoid[F[A]], pt: Pointed[F]): (E >@> F[A]) = {
    def step(acc: F[A])(s: Input[E]): (E >@> F[A]) =
      s(el = e => iter.fold(
        done = (a, _) => cont(step(mon.append(acc, pt.point(a)))),
        cont = k => k(elInput(e)).fold(
          done = (a, _) => cont(step(mon.append(acc, pt.point(a)))),
          cont = (k2) => cont(step(acc)),
          err = e => err(e)
        ),
        err = e => err(e)),
        empty = cont(step(acc)),
        eof = done(acc, eofInput))
    cont(step(mon.z))
  }

  /**
   * Iteratee that collects all inputs with the given monoid.
   */
  def collect[A, F[_]](implicit mon: Monoid[F[A]], pt: Pointed[F]): (A >@> F[A]) = {
    def step(acc: F[A])(s: Input[A]): (A >@> F[A]) =
      s(el = e => cont(step(mon.append(acc, pt.point(e)))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput))
    cont(step(mon.z))
  }

  /**
   * Iteratee that collects all inputs in reverse with the given reducer.
   *
   * This iteratee is useful for F[_] with efficient cons, i.e. List.
   */
  def reversed[A, F[_]](implicit r: Reducer[A, F[A]]): (A >@> F[A]) = {
    def step(acc: F[A])(s: Input[A]): (A >@> F[A]) =
      s(el = e => cont(step(r.cons(e, acc))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput))
    cont(step(r.monoid.z))
  }

}
