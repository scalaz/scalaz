package scalaz

import std.string._
import syntax.monadListen._
 
object MonadWriterExample extends App {
  implicit val monadListen = EitherT.monadListen[Writer, String, String]
 
  case class Person(firstName: String, age: Int)
 
  def notEmpty(s: String)/*: EitherT[({type f[+x] = Writer[String, x]})#f, String, String]*/ = Option(s).filter(x => !x.isEmpty && !x.forall(_.isWhitespace)) match {
    case Some(r) => monadListen.right[String](r)
    case None => monadListen.left[String]("Empty String")
  }
 
  def majority(age: Int)/*: EitherT[({type f[+x] = Writer[String, x]})#f, String, Int]*/ =
    if(age >= 18) monadListen.right[Int](age)
    else monadListen.left[Int]("This person is not an adult")
 
  def validateData(firstName: String, age: Int) /*EitherT[({type f[+x] = Writer[String, x]})#f, String, Person]*/ = for {
    n <- notEmpty(firstName) :++>> (x => "The first name is => " + x + "\n")
    a <- majority(age)       :++>> (x => "She is an adult => " + x + "\n") 
  } yield Person(n, a)
 
  def showMe(v: (Person, String)) {
    println("Log produced during computation -------")
    println(v._2)
    println("Computation result --------------------")
    println(v._1)
  }
 
  println(validateData("", 17).listen.map(showMe).run.run) // showMe will not be called, no log accumulated
  println()
  println()
  println(validateData("Nath", 5).listen.map(showMe).run.run) // showMe will not be called: log value is "Firstname name is => Nath"
  println()
  println()
  validateData("Mimie", 20).listen.map(showMe)
}
