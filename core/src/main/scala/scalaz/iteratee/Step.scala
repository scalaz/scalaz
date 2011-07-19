package scalaz
package iteratee


sealed trait Step[X, E, F[_], A] {
  def fold[Z](
    cont: (Input[E] => IterateeT[X, E, F, A]) => Z
  , done: (=> A, => Input[E]) => Z
  , err: (=> X) => Z
  )
}

object Step extends Steps

trait Steps {
  def cont[X, E, F[_], A](c: Input[E] => IterateeT[X, E, F, A]): Step[X, E, F, A] = new Step[X, E, F, A] {
    def fold[Z](
      cont: (Input[E] => IterateeT[X, E, F, A]) => Z
    , done: (=> A, => Input[E]) => Z
    , err: (=> X) => Z
    ) = cont(c)
  }

  def done[X, E, F[_], A](d: => A, r: => Input[E]): Step[X, E, F, A] = new Step[X, E, F, A] {
    def fold[Z](
      cont: (Input[E] => IterateeT[X, E, F, A]) => Z
    , done: (=> A, => Input[E]) => Z
    , err: (=> X) => Z
    ) = done(d, r)
  }

  def err[X, E, F[_], A](e: => X): Step[X, E, F, A] = new Step[X, E, F, A] {
    def fold[Z](
      cont: (Input[E] => IterateeT[X, E, F, A]) => Z
    , done: (=> A, => Input[E]) => Z
    , err: (=> X) => Z
    ) = err(e)
  }
}
