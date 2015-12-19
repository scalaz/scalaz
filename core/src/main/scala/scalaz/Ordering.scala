package scalaz

/** A ternary marker of how two values relate in an ordering.
  *
  * @note scalaz calls its version of [[scala.math.Ordering]],
  *       [[scalaz.Order]].  This `Ordering` is analogous to the
  *       `Int`s returned by [[scala.math.Ordering]].
  */
sealed abstract class Ordering(val toInt: Int, val name: String) extends Product with Serializable {
  def complement: Ordering
}

object Ordering extends OrderingInstances {
  case object LT extends Ordering(-1, "LT") { def complement = GT }
  case object EQ extends Ordering(0,  "EQ") { def complement = EQ }
  case object GT extends Ordering(1,  "GT") { def complement = LT }

  def fromLessThan[A](a1: A, a2: A)(f: (A, A) => Boolean): Ordering =
    if (f(a1, a2)) LT
    else if (f(a2, a1)) GT
    else EQ

  def fromInt(intOrdering: Int): Ordering = if (intOrdering < 0) LT else if (intOrdering > 0) GT else EQ
}

sealed abstract class OrderingInstances {

  import Ordering._

  implicit val orderingInstance: Enum[Ordering] with Show[Ordering] with Monoid[Ordering] = new Enum[Ordering] with Show[Ordering] with Monoid[Ordering] {
    def order(a1: Ordering, a2: Ordering): Ordering = (a1, a2) match {
      case (LT, LT)      => EQ
      case (LT, EQ | GT) => LT
      case (EQ, LT)      => GT
      case (EQ, EQ)      => EQ
      case (EQ, GT)      => LT
      case (GT, LT | EQ) => GT
      case (GT, GT)      => EQ
    }

    override def shows(f: Ordering) = f.name

    def append(f1: Ordering, f2: => Ordering): Ordering = f1 match {
      case Ordering.EQ => f2
      case o           => o
    }

    def zero: Ordering = Ordering.EQ

    def succ(b: Ordering) = b match {
      case Ordering.LT => Ordering.EQ
      case Ordering.EQ => Ordering.GT
      case Ordering.GT => Ordering.LT
    }
    def pred(b: Ordering) = b match {
      case Ordering.GT => Ordering.EQ
      case Ordering.EQ => Ordering.LT
      case Ordering.LT => Ordering.GT
    }
    override def succn(a: Int, b: Ordering) =
      if(a < 0)
        predn(-a, b)
      else if(a % 3 == 0)
        b
      else if(a % 3 == 1)
        succ(b)
      else
        succ(succ(b))
    override def predn(a: Int, b: Ordering) =
      if(a < 0)
        succn(-a, b)
      else if(a % 3 == 0)
        b
      else if(a % 3 == 1)
        pred(b)
      else
        pred(pred(b))
    override def min = Some(LT)
    override def max = Some(GT)
  }
}
