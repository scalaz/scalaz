package scalaz
package typeclass

trait StrongClass[P[_, _]] extends Strong[P] with ProfunctorClass[P] {
  final def strong: Strong[P] = this
}

object StrongClass {

  trait Template[P[_, _]] extends StrongClass[P] with Strong.First[P]

  trait AltTemplate[P[_, _]] extends StrongClass[P] with Strong.Second[P]

}