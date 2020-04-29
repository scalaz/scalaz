package scalaz

import Id._

package object iteratee {

  type Step[E, A] =
  StepT[E, Id, A]

  type Iteratee[E, A] =
  IterateeT[E, Id, A]

  type Enumerator[E] =
  EnumeratorT[E, Id]

  type Enumeratee[O, I] =
  EnumerateeT[O, I, Id]

  type >@>[E, A] =
  Iteratee[E, A]

}
