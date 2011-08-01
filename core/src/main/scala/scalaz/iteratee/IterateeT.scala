package scalaz
package iteratee

import Input._
import Identity._
import StepT._
import effect._

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
    foldT(k => <=~~[F, Z](cont(k)), (a, o) => <=~~[F, Z](done(a, o)), e => <=~~[F, Z](err(e)))(new Bind[F] {
      def bind[A, B](f: A => F[B]) =
        k => <=~~[F, B](~~=>(f(~~=>(k))))
    })

  def runT(e: (=> X) => F[A])(implicit m: Monad[F]): F[A] = {
    implicit val b = m.bind
    val lifte: (=> X) => IterateeT[X, E, F, A] = x => implicitly[MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ]].lift(e(x))
    m.bd((s: StepT[X, E, F, A]) => s.fold(
      cont = _ => sys.error("diverging iteratee")
      , done = (a, _) => m.point(a)
      , err = e
    ))(>>==(enumEofT(lifte)).value)
  }

  def run(e: (=> X) => A)(implicit i: F =~~= Identity): A =
    runT(x => <=~~[F, A](e(x)))(Monad.monadBP(new Bind[F] {
      def bind[A, B](f: A => F[B]) =
        k => <=~~[F, B](~~=>(f(~~=>(k))))
    }, new Pointed[F] {
      def point[A](a: => A) = i <=~~ Identity.id(a)
    }))

  def flatMap[B](f: A => IterateeT[X, E, F, B])(implicit m: Monad[F]): IterateeT[X, E, F, B] = {
    def through(x: IterateeT[X, E, F, A]): IterateeT[X, E, F, B] =
      iterateeT(
        m.bd((s: StepT[X, E, F, A]) => s.fold[F[StepT[X, E, F, B]]](
          cont = k => m.point(StepT.scont(u => through(k(u))))
          , done = (a, i) =>
            if (i.isEmpty)
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

  def mapI[G[_]](f: F ~> G)(implicit ftr: Functor[F]): IterateeT[X, E, G, A] = {
    def step: StepT[X, E, F, A] => StepT[X, E, G, A] =
      _.fold(
        cont = k => scont[X, E, G, A](k andThen loop)
      , done = (a, i) => sdone[X, E, G, A](a, i)
      , err = e => serr[X, E, G, A](e)
      )
    def loop: IterateeT[X, E, F, A] => IterateeT[X, E, G, A] = i => iterateeT(f(ftr.fmap(step)(i.value)))
    loop(this)
  }
  
  def up[G[_]](implicit pt: Pointed[G], ftr: Functor[F], cpt: CoPointed[F]): IterateeT[X, E, G, A] = {
    mapI(new (F ~> G) {
      def apply[A](a: F[A]) = pt.point(cpt.coPoint(a))
    })
  }
  
  def joinI[I, B](implicit outer: IterateeT[X, E, F, A] =:= IterateeT[X, E, F, StepT[X, I, F, B]], m: Monad[F]): IterateeT[X, E, F, B] = {
    implicit val b = m.bind
    implicit val p = m.pointed
    implicit val ip = IterateeTPointed[X, E, F]
    def check: StepT[X, I, F, B] => IterateeT[X, E, F, B] = _.fold(
      cont = k => k(eofInput) >>== { s => s.mapContOr(_ => sys.error("diverging iteratee"), check(s)) }
    , done = (a, _) => ip.point(a)
    , err = e => err(e)
    )

    outer(this) flatMap check
  }
  
  def %=[O](e: EnumerateeT[X, O, E, F, A])(implicit m: Monad[F]): IterateeT[X, O, F, A] = {
    implicit val b = m.bind
    (this >>== e).joinI[E, A]
  }
  
  /**
   * Feeds input elements to this iteratee until it is done, feeds the produced value to the 
   * inner iteratee.  Then this iteratee will start over, looping until the inner iteratee is done.
   */
  def sequenceI[B](implicit m: Monad[F]): EnumerateeT[X, E, A, F, B] = {
    implicit val p = m.pointed
    implicit val b = m.bind
    def loop: EnumerateeT[X, E, A, F, B] = doneOr(checkEof)
    def checkEof: (Input[A] => IterateeT[X, A, F, B]) => IterateeT[X, E, F, StepT[X, A, F, B]] = k =>
      isEof[X, E, F] flatMap { eof => 
        if (eof)done(scont(k), eofInput)
        else step(k)
      }
    def step: (Input[A] => IterateeT[X, A, F, B]) => IterateeT[X, E, F, StepT[X, A, F, B]] = k =>
      this flatMap (a => k(elInput(a)) >>== loop)
    loop
  }
}

object IterateeT extends IterateeTs {
  def apply[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] =
    iterateeT(s)
}

trait IterateeTs {
  type Iteratee[X, E, A] =
  IterateeT[X, E, Identity, A]

  type Iter[E, F[_], A] =
  IterateeT[Unit, E, F, A]

  type >@>[E, A] =
  Iteratee[Unit, E, A]

  type EnumerateeT[X, O, I, F[_], A] =
  StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]]

  type Enumeratee[X, O, I, A] =
  Step[X, I, A] => Iteratee[X, O, Step[X, I, A]]

  type EnumeratorT[X, E, F[_], A] =
  StepT[X, E, F, A] => IterateeT[X, E, F, A]

  type Enumerator[X, E, A] =
  Step[X, E, A] => Step[X, E, A]

  type >@@>[E, A] =
  Enumerator[Unit, E, A]

  def iterateeT[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] = new IterateeT[X, E, F, A] {
    val value = s
  }

  def iteratee[X, E, A](s: Step[X, E, A]): Iteratee[X, E, A] =
    iterateeT(Identity.id(s))

  def cont[X, E, F[_] : Pointed, A](c: Input[E] => IterateeT[X, E, F, A]): IterateeT[X, E, F, A] =
    iterateeT(implicitly[Pointed[F]].point(StepT.scont(c)))

  def done[X, E, F[_] : Pointed, A](d: => A, r: => Input[E]): IterateeT[X, E, F, A] =
    iterateeT(implicitly[Pointed[F]].point(StepT.sdone(d, r)))

  def err[X, E, F[_] : Pointed, A](e: => X): IterateeT[X, E, F, A] =
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

  implicit def EnumeratorTZero[X, E, F[_]: Pointed, A]: Zero[EnumeratorT[X, E, F, A]] = Zero.zero(_.pointI)
  
  implicit def EnumeratorTSemigroup[X, E, F[_]: Bind, A]: Semigroup[EnumeratorT[X, E, F, A]] =
    Semigroup.semigroup(e1 => e2 => s => e1(s) >>== e2)

  /**An iteratee that consumes the head of the input **/
  def head[E, F[_] : Pointed]: Iter[E, F, Option[E]] = {
    def step(s: Input[E]): Iter[E, F, Option[E]] =
      s(empty = cont(step)
        , el = e => done(Some(e), emptyInput[E])
        , eof = done(None, eofInput[E])
      )
    cont(step)
  }

  def headDoneOr[E, F[_] : Monad, B](b: => B, f: E => Iter[E, F, B]): Iter[E, F, B] = {
    implicit val pt = implicitly[Monad[F]].pointed
    head[E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that returns the first element of the input **/
  def peek[E, F[_] : Pointed]: Iter[E, F, Option[E]] = {
    def step(s: Input[E]): Iter[E, F, Option[E]]
    = s(el = e => done(Some(e), s),
      empty = cont(step),
      eof = done(None, eofInput[E]))
    cont(step)
  }

  def peekDoneOr[E, F[_] : Monad, B](b: => B, f: E => Iter[E, F, B]): Iter[E, F, B] = {
    implicit val pt = implicitly[Monad[F]].pointed
    peek[E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that skips the first n elements of the input **/
  def drop[E, F[_] : Pointed](n: Int): Iter[E, F, Unit] = {
    def step(s: Input[E]): Iter[E, F, Unit] =
      s(el = _ => drop(n - 1),
        empty = cont(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else cont(step)
  }

  /**An iteratee that counts and consumes the elements of the input **/
  def length[E, F[_] : Pointed]: Iter[E, F, Int] = {
    def step(acc: Int)(s: Input[E]): Iter[E, F, Int] =
      s(el = _ => cont(step(acc + 1)),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(0))
  }

  /**
   * An iteratee that checks if the input is EOF.
   */
  def isEof[X, E, F[_]: Pointed]: IterateeT[X, E, F, Boolean] = cont[X, E, F, Boolean](in => done(in.isEof, in))
  
  /**
   * Repeats the given iteratee by appending with the given monoid.
   */
  def repeatBuild[E, A, F[_]](iter: E >@> A)(implicit mon: Monoid[F[A]], pt: Pointed[F]): (E >@> F[A]) = {
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
  
  /**
   * Iteratee that collects inputs with the given monoid until the input element fails a test.
   */
  def takeWhile[A, F[_]](p: A => Boolean)(implicit mon: Monoid[F[A]], pt: Pointed[F]): (A >@> F[A]) = {
    def loop(acc: F[A])(s: Input[A]): (A >@> F[A]) = 
      s(el = e =>
          if (p(e)) cont(loop(mon.append(acc, pt.point(e))))
          else done(acc, s)
      , empty = cont(loop(acc))
      , eof = done(acc, eofInput)
      )
    cont(loop(mon.z))
  }

  def enumEofT[X, E, F[_] : Monad, A](e: (=> X) => IterateeT[X, E, F, A]): EnumeratorT[X, E, F, A] =
    j => {
      implicit val p = implicitly[Monad[F]].pointed
      implicit val b = implicitly[Monad[F]].bind
      j.fold(
        cont = k =>
          k(eofInput) >>== (s =>
            s >- (
                sys.error("diverging iteratee")
                , enumEofT(e) apply s
                , enumEofT(e) apply s
                ))
        , done = (a, _) =>
          StepT.sdone[X, E, F, A](a, eofInput).pointI
        , err = e(_)
      )
    }

  implicit def enumStream[X, E, F[_]: Pointed : Bind, A](xs: Stream[E]): EnumeratorT[X, E, F, A] = { s =>
    xs match {
      case h #:: t => s.mapContOr(_(elInput(h)) >>== enumStream(t), s.pointI)
      case _ => s.pointI
    }
  }
  
  implicit def enumIterator[X, E, A](x: Iterator[E]): EnumeratorT[X, E, IO, A] = {
    def loop: EnumeratorT[X, E, IO, A] = { s =>
      s.mapContOr(
        k =>
          if (x.hasNext) {
            val n = x.next
            k(elInput(n)) >>== loop
          } else s.pointI
      , s.pointI
      )
    }
    loop
  }

  import java.io._
  implicit def enumReader[X, A](r: Reader): EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
    def loop: EnumeratorT[X, IoExceptionOr[Char], IO, A] = { s =>
      s.mapContOr(
        k => {
          val i = IoExceptionOr(r.read)
          if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== loop
          else s.pointI
        }
      , s.pointI
      )
    }
    loop
  }

  def checkCont0[X, E, F[_], A](z: EnumeratorT[X, E, F, A] => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: EnumeratorT[X, E, F, A] = {
      s =>
        s.mapContOr(
          k => z(step)(k)
          , s.pointI
        )
    }
    step
  }

  def checkCont1[S, X, E, F[_], A](z: (S => EnumeratorT[X, E, F, A]) => S => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A], t: S)(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: S => EnumeratorT[X, E, F, A] = {
      o => s =>
        s.mapContOr(
          k => z(step)(o)(k)
          , s.pointI
        )
    }
    step(t)
  }

  def iterate[X, E, F[_] : Monad, A](f: E => E, e: E): EnumeratorT[X, E, F, A] = {
    implicit val bd = implicitly[Monad[F]].bind
    implicit val pt = implicitly[Monad[F]].pointed
    checkCont1[E, X, E, F, A](s => t => k => k(elInput(e)) >>== s(f(t)), e)
  }

  def repeat[X, E, F[_] : Monad, A](e: E): EnumeratorT[X, E, F, A] = {
    implicit val bd = implicitly[Monad[F]].bind
    implicit val pt = implicitly[Monad[F]].pointed
    checkCont0[X, E, F, A](s => k => k(elInput(e)) >>== s)
  }

  def doneOr[X, O, I, F[_]: Pointed, A](f: (Input[I] => IterateeT[X, I, F, A]) => IterateeT[X, O, F, StepT[X, I, F, A]]): EnumerateeT[X, O, I, F, A] = { s =>
    def d: IterateeT[X, O, F, StepT[X, I, F, A]] = done(s, emptyInput)
    s.fold(
      cont = k => f(k)
    , done = (_, _) => d
    , err = _ => d
    )
  }
}
