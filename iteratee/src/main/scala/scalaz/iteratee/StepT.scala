package scalaz
package iteratee

import Iteratee._

/**
 * The current state of an Iteratee, one of:
 *  - '''cont''' Waiting for more data
 *  - '''done''' Already calculated a result
 *
 * @tparam E The type of the input data (mnemonic: '''E'''lement type)
 * @tparam F The type constructor representing an effect.
 *           The type constructor [[scalaz.Id]] is used to model pure computations, and is fixed as such in the type alias [[scalaz.Step]].
 * @tparam A The type of the calculated result
 */
sealed trait StepT[E, F[_], A] {
  def fold[Z](
               cont: (Input[E] => IterateeT[E, F, A]) => Z
               , done: (=> A, => Input[E]) => Z
               ): Z

  /** An alias for `fold` */
  def apply[Z](
                cont: (Input[E] => IterateeT[E, F, A]) => Z
                , done: (=> A, => Input[E]) => Z
                ): Z = fold(cont, done)

  def cont: Option[Input[E] => IterateeT[E, F, A]] =
    fold(
      Some(_)
      , (_, _) => None
    )

  def contOr(k: => Input[E] => IterateeT[E, F, A]): Input[E] => IterateeT[E, F, A] =
    cont getOrElse k

  def mapContOr[Z](k: (Input[E] => IterateeT[E, F, A]) => Z, z: => Z): Z =
    fold(
      k(_)
      , (_, _) => z
    )

  def mapCont(k: (Input[E] => IterateeT[E, F, A]) => IterateeT[E, F, A])(implicit F: Pointed[F]): IterateeT[E, F, A] =
    mapContOr[IterateeT[E, F, A]](k, pointI)

  def doneValue: LazyOption[A] =
    fold(
      _ => LazyOption.lazyNone
      , (a, _) => LazyOption.lazySome(a)
    )

  def doneValueOr(a: => A): A =
    doneValue getOrElse a

  def mapDoneValueOr[Z](k: (=> A) => Z, z: => Z) =
    fold(
      _ => z
      , (a, _) => k(a)
    )

  def doneInput: LazyOption[Input[E]] =
    fold(
      _ => LazyOption.lazyNone
      , (_, i) => LazyOption.lazySome(i)
    )

  def doneInputOr(a: => Input[E]): Input[E] =
    doneInput getOrElse a

  def mapDoneInputOr[Z](k: (=> Input[E]) => Z, z: => Z) =
    fold(
      _ => z
      , (_, i) => k(i)
    )

  def >-[Z](cont: => Z, done: => Z): Z =
    fold(_ => cont, (_, _) => done)

  def pointI(implicit P: Pointed[F]): IterateeT[E, F, A] =
    iterateeT(P.point(this))
}

// object StepT is in the implicit scope for EnumeratorT, so we mix in EnumeratorTInstances here.
object StepT extends StepTFunctions with EnumeratorTInstances {
  private[this] val ToNone: ((=> Any) => None.type) = x => None
  private[this] val ToNone1: (Any => None.type) = x => None
  private[this] val ToNone2: ((=> Any, => Any) => None.type) = (x, y) => None

  object Cont {
    def apply[E, F[_], A](c: Input[E] => IterateeT[E, F, A]): StepT[E, F, A] = new StepT[E, F, A] {
      def fold[Z](
                   cont: (Input[E] => IterateeT[E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   ) = cont(c)
    }

    def unapply[E, F[_], A](s: StepT[E, F, A]): Option[Input[E] => IterateeT[E, F, A]] =
      s.fold(f => Some(f), ToNone2)
  }

  object Done {
    def apply[E, F[_], A](d: => A, r: => Input[E]) = new StepT[E, F, A] {
      def fold[Z](
                   cont: (Input[E] => IterateeT[E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   ) = done(d, r)
    }

    def unapply[E, F[_], A](s: StepT[E, F, A]): Option[(A, Input[E])] =
      s.fold(ToNone1, (a, ie) => Some((a, ie)))
  }
}

trait StepTFunctions {
  def scont[E, F[_], A](c: Input[E] => IterateeT[E, F, A]): StepT[E, F, A] = StepT.Cont(c)
  def sdone[E, F[_], A](d: => A, r: => Input[E]): StepT[E, F, A] = StepT.Done(d, r)
}
