package scalaz
package typeclass

trait ChoiceClass[P[_, _]] extends Choice[P] with ProfunctorClass[P] {
  final def choice: Choice[P] = this
}

object ChoiceClass {

  trait Template[P[_, _]] extends ChoiceClass[P] with Choice.Left[P]

  trait AltTemplate[P[_, _]] extends ChoiceClass[P] with Choice.Right[P]

}