package scalaz.example

object ExampleLogger {
  import scalaz._, Scalaz._

  def main(args: Array[String]) {
    case class Person(name: String, age: Int)

    val a = 7
    val b = "hello"
    val c = Person("Bob", 45)
    val d = a + 100
    val e = b.reverse
    val f = c copy (age = c.age * 10)

    val r =
      (for {
        aa <- a.logger[String] :->> ("starting off with " + _)
        bb <- b.logger[String] :+-> "saying hello"
        cc <- "Hi Bob" <-+: "Creating a person" <-+: c.logger[String]
        dd <- d.logger[String] :+-> "adding a hundred"
      } yield dd) printFlushEachLog

    val s =
        for {
          rr <- r
          ee <- e.logger[String] :+-> "reversing"
          ff <- f.logger[String] :->> ("make Bob a bit older " + _)
        } yield (rr, ff)


    println("FINAL LOG")
    s.printEachLog
  }
}
