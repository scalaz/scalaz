package scalaz.example

object WriterUsage extends App {

  import scalaz._
  import std.list._
  import syntax.writer._
  import syntax.semigroup._ // for |+|
  import syntax.apply._     // for |@|

  type Logger[A] = Writer[List[String], A]

  case class Person(name: String, age: Int, address: Option[Address] = None)
  case class Address(street: String, city: String)

  val drWatson =
    for {
      watson <- Person("Watson", 40).set(List("Create Watson"))
      address <- Address("Baker Street", "London").set(List("Create address."))
      _ <- List("tell lets us log something in between. Writer[List[String], Unit]").tell
      moveWatson <- watson.copy(address = Some(address)).set(List("Move to new address."))
    } yield moveWatson

  // print log
  drWatson.written.map(println)

  // get value
  drWatson.value.map(println)

  val sherlockHolmes =
    for {
      holmes <- Person("Holmes", 40).set(List("Create Holmes"))
      address <- Address("Baker Street", "London").set(List("Create address."))
      moveHolmes <- holmes.copy(address = Some(address)).set(List("Move to new address."))
    } yield (moveHolmes)

  // map lets you map over the value side
  val mapValue: Logger[Option[Address]] = sherlockHolmes.map(x => x.address)
  mapValue.value.map(println)

  // with mapWritten you can map over the written side.
  val mapWritten: Logger[Person] = sherlockHolmes.mapWritten(_.map(entry => "[LOG] " + entry))
  mapWritten.written.map(println)

  // with mapValue you can map over both sides
  val mValue: Logger[Option[Address]] = sherlockHolmes.mapValue { case (log, p) => (log :+ "Extracting address", p.address) }
  mValue.written.map(println)

  // with :++> you can append to the log side of things
  val resultAppend: Logger[Person] = sherlockHolmes :++> List("Finished", "--- new Person ready ---")
  resultAppend.written.map(println)

  // with :++>> you can append using a function
  val resultFappend: Logger[Person] = sherlockHolmes :++>> { x => List("Finished", "--- new Person " + x + " ready ---") }
  resultFappend.written.map(println)

  // <++: and <<++: work like :++>, :++>> only to prepend information
  val resultPrepend: Logger[Person] = sherlockHolmes.<++:(List("Starting to create a Person"))
  resultPrepend.written.map(println)

  // reset your log to zero
  val logNoGood: Logger[Person] = sherlockHolmes.reset
  logNoGood.written.map(println)

  // Writer is an applicative, you can easily combine different results.
  val combined: Logger[List[Person]] = (sherlockHolmes |@| drWatson) { List(_) |+| List(_) }
  combined.written.map(println)
}
