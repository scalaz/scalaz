package scalaz
package std

trait PartialFunctionInstances {
  implicit val partialFunctionInstance: Arrow[PartialFunction] with Category[PartialFunction] with Choice[PartialFunction] = new Arrow[PartialFunction] with Category[PartialFunction] with Choice[PartialFunction] {
    def arr[A, B](f: A => B) = {
      case a => f(a)
    }

    // https://github.com/scala/scala/blob/v2.12.4/src/library/scala/PartialFunction.scala#L195-L218
    private[this] val fallback_pf: PartialFunction[Any, Any] = { case _ => fallback_pf }
    private[this] def checkFallback[B] = fallback_pf.asInstanceOf[PartialFunction[Any, B]]
    private[this] def fallbackOccurred[B](x: B) = (fallback_pf eq x.asInstanceOf[AnyRef])

    def compose[A, B, C](f: PartialFunction[B, C], g: PartialFunction[A, B]): PartialFunction[A, C] = new PartialFunction[A, C] {
      def apply(a: A): C = f(g(a))
      def isDefinedAt(a: A): Boolean = {
        val x: B = g.applyOrElse(a, checkFallback[B])
        if (!fallbackOccurred(x)) {
          f.isDefinedAt(x)
        } else {
          false
        }
      }
      override def applyOrElse[A1 <: A, C1 >: C](a: A1, default: A1 => C1): C1 = {
        val x: B = g.applyOrElse(a, checkFallback[B])
        if (!fallbackOccurred(x)) {
          val y: C = f.applyOrElse(x, checkFallback[C])
          if (!fallbackOccurred(y)) {
            y
          } else {
            default(a)
          }
        } else {
          default(a)
        }
      }
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
