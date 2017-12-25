package scalaz
package typeclass

trait RingClass[A] extends Ring[A] {
  final def ring: Ring[A] = this
}
