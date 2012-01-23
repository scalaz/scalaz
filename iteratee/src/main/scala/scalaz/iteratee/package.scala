package scalaz

package object iteratee {

  type Step[X, E, A] =
  StepT[X, E, Id, A]

  type Iteratee[X, E, A] =
  IterateeT[X, E, Id, A]

  object Iteratee
    extends IterateeFunctions
    with IterateeTFunctions
    with EnumeratorTFunctions
    with EnumerateeTFunctions
    with StepTFunctions
    with InputFunctions {

    def apply[X, E, A](s: Step[X, E, A]): Iteratee[X, E, A] = iteratee(s)
  }

  type Enumerator[X, E] =
  EnumeratorT[X, E, Id]

  type Enumeratee[X, O, I] = 
  EnumerateeT[X, O, I, Id]

  type Iter[E, F[_], A] =
  IterateeT[Unit, E, F, A]

  type >@>[E, A] =
  Iteratee[Unit, E, A]

  type >@@>[E, A] =
  Enumerator[Unit, E]
}
