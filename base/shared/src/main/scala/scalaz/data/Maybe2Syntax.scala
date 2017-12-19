package scalaz
package data

trait Maybe2Syntax {
  def just2 [A, B](a: A, b: B): Maybe2Impl.Maybe2[A, B] = Maybe2Impl.fromOption2(Some2(a, b))
  def empty2[A, B]            : Maybe2Impl.Maybe2[A, B] = None2
}