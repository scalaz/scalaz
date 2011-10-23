package scalaz
package iteratee

import LazyOption._


/**The input to an iteratee. **/
sealed trait Input[E] {
  def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z): Z

  import Input._

  def apply[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
    fold(empty, el, eof)

  def el: LazyOption[E] =
    apply(lazyNone[E], lazySome(_), lazyNone[E])

  def elOr(e: => E) =
    el.getOrElse(e)

  def isEmpty: Boolean =
    apply(true, _ => false, false)

  def isEl: Boolean =
    apply(false, _ => true, false)

  def isEof: Boolean =
    apply(false, _ => false, true)

  def map[X](f: (=> E) => X): Input[X] =
    fold(emptyInput, e => elInput(f(e)), eofInput)

  def flatMap[X](f: (=> E) => Input[X]): Input[X] =
    fold(emptyInput, e => f(e), eofInput)

  def foreach(f: (=> E) => Unit) =
    fold((), e => f(e), ())

  def forall(p: (=> E) => Boolean): Boolean =
    fold(true, p, true)

  def exists(p: (=> E) => Boolean): Boolean =
    fold(false, p, false)

}

object Input extends Inputs {
  def apply[E](e: => E): Input[E] =
    elInput(e)
}

trait Inputs {
  def emptyInput[E]: Input[E] = new Input[E] {
    def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
      empty
  }

  def elInput[E](e: => E): Input[E] = new Input[E] {
    def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
      el(e)
  }

  def eofInput[E]: Input[E] = new Input[E] {
    def fold[Z](empty: => Z, el: (=> E) => Z, eof: => Z) =
      eof
  }

  /*implicit val InputFunctor: Functor[Input] = new Functor[Input] {
    def fmap[A, B](f: A => B) = _ map (e => f(e))
  }

  implicit val InputPointed: Pointed[Input] = new Pointed[Input] {
    def point[A](a: => A) = elInput(a)
  }

  implicit val InputApplic: Applic[Input] = new Applic[Input] {
    def applic[A, B](f: Input[A => B]) =
      a => f flatMap (k => a map (k apply _))
  }

  implicit val InputBind: Bind[Input] = new Bind[Input] {
    def bind[A, B](f: A => Input[B]) =
      _ flatMap (e => f(e))
  }

  implicit val InputJoin: Join[Input] = new Join[Input] {
    def join[A] =
      _ flatMap (x => x)
  }

  implicit val InputEach: Each[Input] = new Each[Input] {
    def each[A](f: A => Unit) =
      _ foreach (e => f(e))
  }

  implicit val InputFoldl: Foldl[Input] = new Foldl[Input] {
    def foldl[A, B] =
      f => z => _.fold(
        empty = z
        , el = a => f(z)(a)
        , eof = z
      )
  }

  implicit val InputFoldr: Foldr[Input] = new Foldr[Input] {
    def foldr[A, B] =
      f => z => _.fold(
        empty = z
        , el = a => f(a)(z)
        , eof = z
      )
  }

  implicit val InputTraverse: Traverse[Input] = new Traverse[Input] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      _.fold(
        empty = implicitly[Applicative[F]].point(emptyInput[B])
        , el = x => implicitly[Applicative[F]].fmap((b: B) => elInput(b))(f(x))
        , eof = implicitly[Applicative[F]].point(eofInput[B])
      )
  }

  implicit val InputIndex: Index[Input] = new Index[Input] {
    def index[A](a: Input[A]) =
      n => if (n == 1) a.fold(
        empty = None
        , el = a => Some(a)
        , eof = None
      )
      else None
  }

  implicit val InputLength: Length[Input] = new Length[Input] {
    def len[A](a: Input[A]) = a.fold(
      empty = 0
      , el = _ => 1
      , eof = 0
    )
  }

  implicit val InputEmpty: Empty[Input] = new Empty[Input] {
    def empty[A] = emptyInput
  }

  implicit val InputPlus: Plus[Input] = new Plus[Input] {
    def plus[A](a1: Input[A], a2: => Input[A]) =
      a1.fold(
        empty = a2
        , el = _ => a1
        , eof = a2
      )
  }

  implicit val InputApplicative: Applicative[Input] =
    Applicative.applicative

  implicit val InputApplicFunctor: ApplicFunctor[Input] =
    ApplicFunctor.applicFunctor

  implicit val InputBindFunctor: BindFunctor[Input] =
    BindFunctor.bindFunctor

  implicit val InputMonad: Monad[Input] =
    Monad.monadBP

  implicit val InputMonadEmpty: MonadEmpty[Input] =
    MonadEmpty.monadEmpty

  implicit val InputMonadEmptyPlus: MonadEmptyPlus[Input] =
    MonadEmptyPlus.monadEmptyPlus

  implicit val InputPointedEmpty: PointedEmpty[Input] =
    PointedEmpty.pointedEmpty

  implicit val InputPointedFunctor: PointedFunctor[Input] =
    PointedFunctor.pointedFunctor

  implicit def InputEqual[A: Equal]: Equal[Input[A]] =
    Equal.equal(a1 => a2 => a1.fold(
      empty = a2.isEmpty
      , el = a => a2.exists(z => implicitly[Equal[A]].equal(a)(z))
      , eof = a2.isEmpty
    ))

  implicit def InputShow[A: Show]: Show[Input[A]] =
    Show.shows(_.fold(
      empty = "empty-input"
      , el = a => "el-input(" + implicitly[Show[A]].shows(a) + ")"
      , eof = "eof-input"
    ))

  implicit def InputZero[A]: Zero[Input[A]] =
    Zero.zero(emptyInput)

  implicit def InputSemigroup[A: Semigroup]: Semigroup[Input[A]] =
    Semigroup.semigroup(a1 => a2 => a1.fold(
      empty = a2.fold(
        empty = emptyInput
        , el = elInput
        , eof = eofInput
      )
      , el = xa => a2.fold(
        empty = elInput(xa)
        , el = ya => elInput(implicitly[Semigroup[A]].append(xa, ya))
        , eof = eofInput
      )
      , eof = eofInput
    ))

  implicit def InputMonoid[A: Semigroup]: Monoid[Input[A]] =
    Monoid.monoid

  implicit def InputIsZero[A]: IsZero[Input[A]] =
    IsZero.isZero(_.isEmpty)*/
}
