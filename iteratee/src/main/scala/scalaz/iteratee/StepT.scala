package scalaz
package iteratee

import Iteratee._

/**
 * The current state of an Iteratee, one of:
 *  - '''cont''' Waiting for more data
 *  - '''done''' Already calculated a result
 *  - '''err''' Error, unable to calculate a result
 *
 * @tparam X The type of the error (mnemonic: e'''X'''ception type)
 * @tparam E The type of the input data (mnemonic: '''E'''lement type)
 * @tparam F The type constructor representing an effect.
 *           The type constructor [[scalaz.Id]] is used to model pure computations, and is fixed as such in the type alias [[scalaz.Step]].
 * @tparam A The type of the calculated result
 */
sealed trait StepT[X, E, F[_], A] {
  def fold[Z](
               cont: (Input[E] => IterateeT[X, E, F, A]) => Z
               , done: (=> A, => Input[E]) => Z
               , err: (=> X) => Z
               ): Z

  /** An alias for `fold` */
  def apply[Z](
                cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                , done: (=> A, => Input[E]) => Z
                , err: (=> X) => Z
                ): Z = fold(cont, done, err)

  def cont: Option[Input[E] => IterateeT[X, E, F, A]] =
    fold(
      Some(_)
      , (_, _) => None
      , _ => None
    )

  def contOr(k: => Input[E] => IterateeT[X, E, F, A]): Input[E] => IterateeT[X, E, F, A] =
    cont getOrElse k

  def mapContOr[Z](k: (Input[E] => IterateeT[X, E, F, A]) => Z, z: => Z) =
    fold(
      k(_)
      , (_, _) => z
      , _ => z
    )

  def mapCont(k: (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit F: Pointed[F]) =
    mapContOr[IterateeT[X, E, F, A]](k, pointI)

  def doneValue: LazyOption[A] =
    fold(
      _ => LazyOption.lazyNone
      , (a, _) => LazyOption.lazySome(a)
      , _ => LazyOption.lazyNone
    )

  def doneValueOr(a: => A): A =
    doneValue getOrElse a

  def mapDoneValueOr[Z](k: (=> A) => Z, z: => Z) =
    fold(
      _ => z
      , (a, _) => k(a)
      , _ => z
    )

  def doneInput: LazyOption[Input[E]] =
    fold(
      _ => LazyOption.lazyNone
      , (_, i) => LazyOption.lazySome(i)
      , _ => LazyOption.lazyNone
    )

  def doneInputOr(a: => Input[E]): Input[E] =
    doneInput getOrElse a

  def mapDoneInputOr[Z](k: (=> Input[E]) => Z, z: => Z) =
    fold(
      _ => z
      , (_, i) => k(i)
      , _ => z
    )

  def err: LazyOption[X] =
    fold(
      _ => LazyOption.lazyNone
      , (_, _) => LazyOption.lazyNone
      , LazyOption.lazySome(_)
    )

  def errOr(x: => X): X =
    err getOrElse x

  def mapErrOr[Z](k: (=> X) => Z, z: => Z) =
    fold(
      _ => z
      , (_, _) => z
      , k
    )

  def >-[Z](cont: => Z, done: => Z, err: => Z): Z =
    fold(_ => cont, (_, _) => done, _ => err)

  def pointI(implicit P: Pointed[F]): IterateeT[X, E, F, A] =
    iterateeT(P.point(this))
}

// object StepT is in the implicit scope for EnumeratorT, so we mix in EnumeratorTInstances here.
object StepT extends StepTFunctions with EnumeratorTInstances {
  private[this] val ToNone: ((=> Any) => None.type) = x => None
  private[this] val ToNone1: (Any => None.type) = x => None
  private[this] val ToNone2: ((=> Any, => Any) => None.type) = (x, y) => None

  object Cont {
    def apply[X, E, F[_], A](c: Input[E] => IterateeT[X, E, F, A]): StepT[X, E, F, A] = new StepT[X, E, F, A] {
      def fold[Z](
                   cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   , err: (=> X) => Z
                   ) = cont(c)
    }

    def unapply[X, E, F[_], A](s: StepT[X, E, F, A]): Option[Input[E] => IterateeT[X, E, F, A]] =
      s.fold(f => Some(f), ToNone2, ToNone)
  }

  object Done {
    def apply[X, E, F[_], A](d: => A, r: => Input[E]) = new StepT[X, E, F, A] {
      def fold[Z](
                   cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   , err: (=> X) => Z
                   ) = done(d, r)
    }

    def unapply[X, E, F[_], A](s: StepT[X, E, F, A]): Option[(A, Input[E])] =
      s.fold(ToNone1, (a, ie) => Some((a, ie)), ToNone)
  }

  object Err {
    def apply[X, E, F[_], A](e: => X): StepT[X, E, F, A] = new StepT[X, E, F, A] {
      def fold[Z](
                   cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   , err: (=> X) => Z
                   ) = err(e)
    }

    def unapply[X, E, F[_], A](s: StepT[X, E, F, A]): Option[X] =
      s.fold(ToNone1, ToNone2, Some(_))
  }
}

trait StepTFunctions {
  def scont[X, E, F[_], A](c: Input[E] => IterateeT[X, E, F, A]): StepT[X, E, F, A] = StepT.Cont(c)
  def sdone[X, E, F[_], A](d: => A, r: => Input[E]): StepT[X, E, F, A] = StepT.Done(d, r)
  def serr[X, E, F[_], A](e: => X): StepT[X, E, F, A] = StepT.Err(e)
}
