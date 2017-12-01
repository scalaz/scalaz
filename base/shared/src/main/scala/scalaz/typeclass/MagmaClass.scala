package scalaz
package typeclass

trait MagmaClass[A] extends Magma[A]{
  final def magma: Magma[A] = this
}
