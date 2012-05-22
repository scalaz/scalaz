package toto

import scalaz._
import scalaz.std.string._
import scalaz.MonadWriter
import scalaz.ValidationT
import scalaz.WriterT._
import scala.Math._

object Toto extends App {
  val MW = ValidationT.validationTMonadWriter[Writer, String, String]

  def squareroot(x: Double) =
    if(x < 0) MW.failureT[Double]("Can't take squaroot of negative number")
    else MW.successT[Double](sqrt(x))

  def inverse(x: Double) =
    if(x == 0) MW.failureT[Double]("Can't take inverse of zero")
    else MW.successT[Double](1/x)

  def resultat(x: Double) = for {
    y <- squareroot(x)
    _ <- MW.tell("SquareRoot ok, ")
    z <- inverse(y)
    _ <- MW.tell("Inverse ok") 
  } yield z

  println("0: " + resultat(0.0).run.run)
  println("-1: " + resultat(-1.0).run.run)
  println("4: " + resultat(4).run.run)
  
}