package scalaz

import scalaz.syntax.equal._
import scalaz.syntax.show._

sealed trait Either3[+A, +B, +C] {
  def fold[Z](left: A => Z, middle: B => Z, right: C => Z): Z = this match {
    case Left3(a)   => left(a)
    case Middle3(b) => middle(b)
    case Right3(c)  => right(c)
  }

  def eitherLeft:  Either[Either[A, B], C] = this match {
    case Left3(a)   => Left(Left(a))
    case Middle3(b) => Left(Right(b))
    case Right3(c)  => Right(c)
  }

  def eitherRight: Either[A, Either[B, C]] = this match {
    case Left3(a)   => Left(a)
    case Middle3(b) => Right(Left(b))
    case Right3(c)  => Right(Right(c))
  }

  def leftOr[Z](z: => Z)(f: A => Z)   = fold(f, _ => z, _ => z)
  def middleOr[Z](z: => Z)(f: B => Z) = fold(_ => z, f, _ => z)
  def rightOr[Z](z: => Z)(f: C => Z)  = fold(_ => z, _ => z, f)
}

case class Left3[+A, +B, +C](a: A) extends Either3[A, B, C]
case class Middle3[+A, +B, +C](b: B) extends Either3[A, B, C]
case class Right3[+A, +B, +C](c: C) extends Either3[A, B, C]

object Either3 {
  def left3[A, B, C](a: A):   Either3[A, B, C] = Left3(a)
  def middle3[A, B, C](b: B): Either3[A, B, C] = Middle3(b)
  def right3[A, B, C](c: C):  Either3[A, B, C] = Right3(c)

  implicit def equal[A: Equal, B: Equal, C: Equal]: Equal[Either3[A, B, C]] = new Equal[Either3[A, B, C]] {
    def equal(e1: Either3[A, B, C], e2: Either3[A, B, C]) = (e1, e2) match {
      case (Left3(a1),   Left3(a2))   => a1 === a2
      case (Middle3(b1), Middle3(b2)) => b1 === b2
      case (Right3(c1),  Right3(c2))  => c1 === c2
      case _ => false
    }
  }

  implicit def show[A: Show, B: Show, C: Show]: Show[Either3[A, B, C]] = new Show[Either3[A, B, C]] {
    override def show(v: Either3[A, B, C]) = v match {
      case Left3(a)   => Cord("Left3(", a.shows, ")")
      case Middle3(b) => Cord("Middle3(", b.shows, ")")
      case Right3(c)  => Cord("Right3(", c.shows, ")")
    }
  }
}


// vim: set ts=4 sw=4 et:
