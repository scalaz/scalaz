package scalaz
package std

object PartialFunction {
  val partialFunction = new Arr[PartialFunction] {
    def arr[A, B](f: (A) => B): PartialFunction[A, B] = {
      case a => f(a)
    }
  }
}