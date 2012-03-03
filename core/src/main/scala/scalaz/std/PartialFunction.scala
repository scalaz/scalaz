package scalaz
package std

trait PartialFunctionInstances {
  val partialFunctionInstance = new Arrow[PartialFunction] with Category[PartialFunction] with Choice[PartialFunction] with Split[PartialFunction] {
    def arr[A, B](f: (A) => B) = {
      case a => f(a)
    }
    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]) = new PartialFunction[A, C] {
      def apply(a: A): C = f(g(a))
      def isDefinedAt(a: A): Boolean = g.isDefinedAt(a) && f.isDefinedAt(g(a))
    }

    def id[A] = {
      case a => a
    }
    
    def choice[A, B, C](f: => PartialFunction[A, C], g: => PartialFunction[B, C]): PartialFunction[Either[A,  B], C] = {
      case Left(a) => f(a)
      case Right(b) => g(b)
    }
    
    def split[A, B, C, D](f: PartialFunction[A, B], g: PartialFunction[C, D]): PartialFunction[(A,  C), (B, D)] = {
      case (a, c) => (f(a), g(c))
    }
                    
    def first[A, B, C](f: PartialFunction[A, B]): PartialFunction[(A, C), (B, C)] = {
      case (a, c) if f.isDefinedAt(a) => (f(a), c)
    }
  }
}

object partialFunction extends PartialFunctionInstances