package scalaz
package typeclass

trait AbelianGroupClass[A] extends AbelianGroup[A] with GroupClass[A]{
  final def abelianGroup: AbelianGroup[A] = this
}
