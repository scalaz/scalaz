package scalaz.example

object WriterUsage extends App {

  import scalaz._
  import std.list._
  import syntax.writer._
  import syntax.id._
  
  import syntax.each._      // for foreach
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
    } yield moveWatson                            //> drWatson  : scalaz.WriterT[scalaz.Id.Id,List[String],scalaz.example.WriterUs
                                                  //| age.Person] = scalaz.WriterTFunctions$$anon$23@530612ba

  // print log
  drWatson.written.foreach(println)               //> Create Watson
                                                  //| Create address.
                                                  //| tell lets us log something in between. Writer[List[String], Unit]
                                                  //| Move to new address.
  // get value
  drWatson.value.foreach(println)                 //> Person(Watson,40,Some(Address(Baker Street,London)))

  val sherlockHolmes =
    for {
      holmes <- Person("Holmes", 40).set(List("Create Holmes"))
      address <- Address("Baker Street", "London").set(List("Create address."))
      moveHolmes <- holmes.copy(address = Some(address)).set(List("Move to new address."))
    } yield (moveHolmes)                          //> sherlockHolmes  : scalaz.WriterT[scalaz.Id.Id,List[String],scalaz.example.W
                                                  //| riterUsage.Person] = scalaz.WriterTFunctions$$anon$23@17776a8

  // map lets you map over the value side
  val mapValue: Logger[Option[Address]] = sherlockHolmes.map(x => x.address)
                                                  //> mapValue  : scalaz.example.WriterUsage.Logger[Option[scalaz.example.WriterU
                                                  //| sage.Address]] = scalaz.WriterTFunctions$$anon$23@69a10787
  mapValue.value.foreach(println)                 //> Address(Baker Street,London)

  // with mapWritten you can map over the written side.
  val mapWritten: Logger[Person] = sherlockHolmes.mapWritten(_.map(entry => s"[LOG] $entry"))
                                                  //> mapWritten  : scalaz.example.WriterUsage.Logger[scalaz.example.WriterUsage.
                                                  //| Person] = scalaz.WriterTFunctions$$anon$23@4ba2ca36
  mapWritten.written.foreach(println)             //> [LOG] Create Holmes
                                                  //| [LOG] Create address.
                                                  //| [LOG] Move to new address.

  // with mapValue you can map over both sides
  val mValue: Logger[Option[Address]] = sherlockHolmes.mapValue { case (log, p) => (log :+ "Extracting address", p.address) }
                                                  //> mValue  : scalaz.example.WriterUsage.Logger[Option[scalaz.example.WriterUsa
                                                  //| ge.Address]] = scalaz.WriterTFunctions$$anon$23@3444d69d
  mValue.written.foreach(println)                 //> Create Holmes
                                                  //| Create address.
                                                  //| Move to new address.
                                                  //| Extracting address

  // with :++> you can append to the log side of things
  val resultAppend: Logger[Person] = sherlockHolmes :++> List("Finished", "--- new Person ready ---")
                                                  //> resultAppend  : scalaz.example.WriterUsage.Logger[scalaz.example.WriterUsag
                                                  //| e.Person] = scalaz.WriterTFunctions$$anon$23@37574691
  resultAppend.written.foreach(println)           //> Create Holmes
                                                  //| Create address.
                                                  //| Move to new address.
                                                  //| Finished
                                                  //| --- new Person ready ---

  // with :++>> you can append using a function
  val resultFappend: Logger[Person] = sherlockHolmes :++>> { x => List("Finished", s"--- new Person $x ready ---") }
                                                  //> resultFappend  : scalaz.example.WriterUsage.Logger[scalaz.example.WriterUsa
                                                  //| ge.Person] = scalaz.WriterTFunctions$$anon$23@1445d7f
  resultFappend.written.foreach(println)          //> Create Holmes
                                                  //| Create address.
                                                  //| Move to new address.
                                                  //| Finished
                                                  //| --- new Person Person(Holmes,40,Some(Address(Baker Street,London))) ready -
                                                  //| --
  // <++: and <<++: work like :++>, :++>> only to prepend information
  val resultPrepend: Logger[Person] = sherlockHolmes.<++:(List("Starting to create a Person"))
                                                  //> resultPrepend  : scalaz.example.WriterUsage.Logger[scalaz.example.WriterUsa
                                                  //| ge.Person] = scalaz.WriterTFunctions$$anon$23@10b48321
  resultPrepend.written.foreach(println)          //> Starting to create a Person
                                                  //| Create Holmes
                                                  //| Create address.
                                                  //| Move to new address.

  // rest your log to zero
  val logNoGood: Logger[Person] = sherlockHolmes.reset
                                                  //> logNoGood  : scalaz.example.WriterUsage.Logger[scalaz.example.WriterUsage.P
                                                  //| erson] = scalaz.WriterTFunctions$$anon$23@453da22c
  logNoGood.written.foreach(println)

  // Writer is an applicative, you can easily combine different results.
  val combined: Logger[List[Person]] = (sherlockHolmes |@| drWatson) { List(_) |+| List(_) }
                                                  //> combined  : scalaz.example.WriterUsage.Logger[List[scalaz.example.WriterUsa
                                                  //| ge.Person]] = scalaz.WriterTFunctions$$anon$23@1f1c7bf6
  combined.written.foreach(println)               //> Create Holmes
                                                  //| Create address.
                                                  //| Move to new address.
                                                  //| Create Watson
                                                  //| Create address.
                                                  //| tell lets us log something in between. Writer[List[String], Unit]
                                                  //| Move to new address.

}
  
