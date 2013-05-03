package scalaz.example

object WriterUsage extends App {
  import scalaz._
  import std.list._
  import syntax.writer._

  case class Person(name: String, age: Int)

  val a = 7
  val b = "hello"
  val c = Person("Bob", 45)
  val d = a + 100
  val e = b.reverse
  val f = c copy (age = c.age * 10)

  val r =
    for {
      _ ← List("starting off with " + a).tell
      bb = b
      _ ← List("saying hello").tell
      cc = c
      _ ← List("Creating a person " + cc, "Hi Bob").tell
      dd = d
      _ ← List("adding a hundred").tell
    } yield dd

  val s =
    for {
      rr ← r
      ee = e
      _ ← List("reversing").tell
      ff = f
      _ ← List("make Bob a bit older " + ff).tell
    } yield (rr, ee, ff)

  println("FINAL LOG")
  s.written.foreach(println)
  println()
  println("FINAL RESULT")
  println(s.value)
}
