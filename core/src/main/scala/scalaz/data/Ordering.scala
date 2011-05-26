package scalaz
package data

sealed trait Ordering {
  val toInt: Int

  def compare(o: => Ordering): Ordering =
    this match {
      case EQ => o
      case LT => LT
      case GT => GT
    }
}

case object LT extends Ordering {
  val toInt = -1
}

case object EQ extends Ordering {
  val toInt = 0
}

case object GT extends Ordering {
  val toInt = 1
}

object Ordering extends Orderings

trait Orderings {

  implicit val OrderingZero: Zero[Ordering] =
    Zero.zero(EQ)

  implicit val OrderingSemigroup: Semigroup[Ordering] = new Semigroup[Ordering] {
    def append(a1: Ordering, a2: => Ordering) =
      a1 compare a2
  }

  implicit val OrderingMonoid: Monoid[Ordering] =
    Monoid.monoid

  implicit val OrderingShow: Show[Ordering] =
    Show.showA

  implicit val OrderingEqual: Equal[Ordering] =
    Equal.equalBy(_.toInt)

  implicit def OrderingContravariant: Contravariant[scala.Ordering] = new Contravariant[scala.Ordering] {
    def contramap[A, B](f: B => A) =
      _ on f
  }

}