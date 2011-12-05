package scalaz

sealed abstract class Ordering(val toInt: Int, val name: String)

object Ordering extends OrderingFunctions with OrderingInstances {
  case object LT extends Ordering(-1, "LT")
  case object EQ extends Ordering(0, "EQ")
  case object GT extends Ordering(1, "GT")
}

trait OrderingInstances {

  import Ordering._

  implicit val orderingInstance: Order[Ordering] with Show[Ordering] with Monoid[Ordering] = new Order[Ordering] with Show[Ordering] with Monoid[Ordering] {
    def order(a1: Ordering, a2: Ordering): Ordering = (a1, a2) match {
      case (LT, LT)      => EQ
      case (LT, EQ | GT) => LT
      case (EQ, LT)      => GT
      case (EQ, EQ)      => EQ
      case (EQ, GT)      => LT
      case (GT, LT | EQ) => GT
      case (GT, GT)      => EQ
    }

    def show(f: Ordering): List[Char] = f.name.toList

    def append(f1: Ordering, f2: => Ordering): Ordering = f1 match {
      case Ordering.EQ => f2
      case o           => o
    }

    def zero: Ordering = Ordering.EQ
  }
}

trait OrderingFunctions {
  import Ordering._

  def fromLessThan[A](a1: A, a2: A)(f: (A, A) => Boolean): Ordering =
    if (f(a1, a2)) LT
    else if (f(a2, a1)) GT
    else EQ

  def fromInt(intOrdering: Int) = if (intOrdering < 0) LT else if (intOrdering > 0) GT else EQ
}
