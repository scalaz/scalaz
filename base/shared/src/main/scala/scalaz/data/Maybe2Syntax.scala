package scalaz
package data

trait Maybe2Syntax {

  def empty2[A, B] = Maybe2.empty2[A, B]
  def just2[A, B](a: A, b: B) = Maybe2.just2(a, b)
}
