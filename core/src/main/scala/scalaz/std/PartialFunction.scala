package scalaz
package std

trait PartialFunctionInstances {
  val partialFunctionInstance = new Arr[PartialFunction] with Category[PartialFunction] {
    def arr[A, B](f: (A) => B): PartialFunction[A, B] = {
      case a => f(a)
    }
    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]): PartialFunction[A, C] = new PartialFunction[A, C] {
      def apply(a: A): C = f(g(a))
      def isDefinedAt(a: A): Boolean = g.isDefinedAt(a) && f.isDefinedAt(g(a))
    }

    def id[A]: PartialFunction[A, A] = null
  }
}

object partialFunction extends PartialFunctionInstances