package scalaz
package typeclass

trait GroupClass[A] extends Group[A] with MonoidClass[A]{
  final def group: Group[A] = this
}
