package scalaz

sealed abstract class Ordering(val toInt: Int, val name: String)

object Ordering {
  case object LT extends Ordering(-1, "LT")
  case object EQ extends Ordering(0, "EQ")
  case object GT extends Ordering(1, "GT")
  
  implicit object ordering extends Order[Ordering] with Show[Ordering] {
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
  }
  
  def fromLessThan[A](a1: A, a2: A)(f: (A, A) => Boolean): Ordering = 
     if (f(a1, a2)) LT
     else if (f(a2, a1)) GT
     else EQ

  def fromInt(intOrdering: Int) = intOrdering match {
    case -1 => LT
    case 0  => EQ
    case 1  => GT
  }
}
