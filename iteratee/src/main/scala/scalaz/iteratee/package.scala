package scalaz

package object iteratee {

  type Step[E, A] =
  StepT[E, Id, A]

  type Iteratee[E, A] =
  IterateeT[E, Id, A]

  object Iteratee
    extends IterateeFunctions
    with IterateeTFunctions
    with EnumeratorTFunctions
    with EnumeratorPFunctions
    with EnumerateeTFunctions
    with StepTFunctions
    with InputFunctions {

    def apply[E, A](s: Step[E, A]): Iteratee[E, A] = iteratee(s)
  }

  type Enumerator[E] =
  EnumeratorT[E, Id]

  type Enumeratee[O, I] =
  EnumerateeT[O, I, Id]

  type >@>[E, A] =
  Iteratee[E, A]

}
