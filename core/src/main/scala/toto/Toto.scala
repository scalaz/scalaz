package toto

import scalaz._
import scalaz.std.string._
import scalaz.MonadWriter
import scalaz.ValidationT
import scalaz.WriterT._
import scala.Math._

object Toto extends App {
  val MW = ValidationT.validationTMonadWriter[Writer, String, String]
  
  case class Person(firstName: String, age: Int)

  def notEmpty(s: String) = Option(s).filter(x => !x.isEmpty && !x.forall(_.isWhitespace)) match {
    case Some(r) => MW.successT[String](r)
    case None    => MW.failureT[String]("Empty String")
  }

  def majority(age: Int) =
    if(age >= 18) MW.successT[Int](age)
    else MW.failureT[Int]("This person is not an adult")

  def validateData(firstName: String, age: Int) = for {
    n <- notEmpty(firstName)
    _ <- MW.tell("The first name is => " + n + " \n")
    a <- majority(age)
    _ <- MW.tell("She is an adult => " + a + " \n") 
  } yield Person(n, a)

  def showMe(v: (Person, String)) {
    println("Log produced during computation -------")
    println(v._2)
    println("Computation result --------------------")
    println(v._1)
  }

  println(MW.listen(validateData("", 17)).map(showMe).run.run) // showMe will not be called, no log accumulated
  println()
  println()
  println(MW.listen(validateData("Nath", 5)).map(showMe).run.run) // showMe will not be called: log value is "Firstname name is => Nath" 
  println()
  println()
  MW.listen(validateData("Mimie", 20)).map(showMe)
}
