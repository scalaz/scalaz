package scalaz
package typeclass

trait ChoiceClass[P[_, _]] extends Choice[P] with ProfunctorClass[P] {
  final def choice: Choice[P] = this
}