package scalaz

import scalaz.syntax.equal._
import scalaz.syntax.show._

sealed abstract class Either3[A, B, C] extends Product with Serializable {
  def fold[Z](left: A => Z, middle: B => Z, right: C => Z): Z = this match {
    case Left3(a)   => left(a)
    case Middle3(b) => middle(b)
    case Right3(c)  => right(c)
  }

  def eitherLeft: (A \/ B) \/ C = this match {
    case Left3(a)   => -\/(-\/(a))
    case Middle3(b) => -\/(\/-(b))
    case Right3(c)  => \/-(c)
  }

  def eitherRight: A \/ (B \/ C) = this match {
    case Left3(a)   => -\/(a)
    case Middle3(b) => \/-(-\/(b))
    case Right3(c)  => \/-(\/-(c))
  }

  def leftOr[Z](z: => Z)(f: A => Z): Z = fold(f, _ => z, _ => z)
  def middleOr[Z](z: => Z)(f: B => Z): Z = fold(_ => z, f, _ => z)
  def rightOr[Z](z: => Z)(f: C => Z): Z = fold(_ => z, _ => z, f)
}

final case class Left3[A, B, C](a: A) extends Either3[A, B, C]
final case class Middle3[A, B, C](b: B) extends Either3[A, B, C]
final case class Right3[A, B, C](c: C) extends Either3[A, B, C]

object Either3 {
  def left3[A, B, C](a: A):   Either3[A, B, C] = Left3(a)
  def middle3[A, B, C](b: B): Either3[A, B, C] = Middle3(b)
  def right3[A, B, C](c: C):  Either3[A, B, C] = Right3(c)

  implicit def equal[A: Equal, B: Equal, C: Equal]: Equal[Either3[A, B, C]] = {
    case (Left3(a1), Left3(a2)) => a1 === a2
    case (Middle3(b1), Middle3(b2)) => b1 === b2
    case (Right3(c1), Right3(c2)) => c1 === c2
    case _ => false
  }

  implicit def show[A: Show, B: Show, C: Show]: Show[Either3[A, B, C]] = {
    case Left3(a) => cord"Left3($a)"
    case Middle3(b) => cord"Middle3($b)"
    case Right3(c) => cord"Right3($c)"
  }
}


// vim: set ts=4 sw=4 et:
