package scalaz

// TODO not so sure about mixing all the functions into the package object.
package object iteratee
  extends IterateeTFunctions
  with Iteratees
  with EnumeratorTFunctions
  with EnumerateeTs
  with StepTFunctions
  with Inputs {

  type Step[X, E, A] =
  StepT[X, E, Id, A]

  type Iteratee[X, E, A] =
  IterateeT[X, E, Id, A]

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
}
