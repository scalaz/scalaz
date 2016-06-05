package scalaz
package std

trait PartialFunctionInstances {
  implicit val partialFunctionInstance = new Arrow[PartialFunction] with Category[PartialFunction] with Choice[PartialFunction] {
    def arr[A, B](f: A => B) = {
      case a => f(a)
    }
    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]) = new PartialFunction[A, C] {
      def apply(a: A): C = f(g(a))
      def isDefinedAt(a: A): Boolean = g.isDefinedAt(a) && f.isDefinedAt(g(a))
    }

    def id[A] = {
      case a => a
    }

    def choice[A, B, C](f: => PartialFunction[A, C], g: => PartialFunction[B, C]): PartialFunction[A \/ B, C] = {
      case -\/(a) if f isDefinedAt a => f(a)
      case \/-(b) if g isDefinedAt b => g(b)
    }

    override def split[A, B, C, D](f: PartialFunction[A, B], g: PartialFunction[C, D]): PartialFunction[(A,  C), (B, D)] = {
      case (a, c) if f.isDefinedAt(a) && g.isDefinedAt(c) => (f(a), g(c))
    }

    def first[A, B, C](f: PartialFunction[A, B]): PartialFunction[(A, C), (B, C)] = {
      case (a, c) if f.isDefinedAt(a) => (f(a), c)
    }
  }
}

object partialFunction extends PartialFunctionInstances
