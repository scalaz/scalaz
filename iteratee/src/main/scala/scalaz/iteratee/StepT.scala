package scalaz
package iteratee

sealed trait StepT[X, E, F[_], A] {
  def fold[Z](
                 cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                 , done: (=> A, => Input[E]) => Z
                 , err: (=> X) => Z
                 ): Z

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

  import IterateeT._

  def pointI(implicit P: Pointed[F]): IterateeT[X, E, F, A] =
    iterateeT(P.pure(this))
}

object StepT extends StepTs

trait StepTs {
  type Step[X, E, A] =
  StepT[X, E, Id, A]

  def scont[X, E, F[_], A](c: Input[E] => IterateeT[X, E, F, A]): StepT[X, E, F, A] = new StepT[X, E, F, A] {
    def fold[Z](
                   cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   , err: (=> X) => Z
                   ) = cont(c)
  }

  def sdone[X, E, F[_], A](d: => A, r: => Input[E]): StepT[X, E, F, A] = new StepT[X, E, F, A] {
    def fold[Z](
                   cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   , err: (=> X) => Z
                   ) = done(d, r)
  }

  def serr[X, E, F[_], A](e: => X): StepT[X, E, F, A] = new StepT[X, E, F, A] {
    def fold[Z](
                   cont: (Input[E] => IterateeT[X, E, F, A]) => Z
                   , done: (=> A, => Input[E]) => Z
                   , err: (=> X) => Z
                   ) = err(e)
  }
}
