package scalaz.example


object DirectTypeClassUsage extends App {

  val o1: Option[Int] = Some(0)
  val o2: Option[Option[Int]] = Some(Some(0))
  val l1: List[String] = List("one")
  val l2: List[List[String]] = List(List("one"))

  direct1()
  direct2()

  // Direct use of type class for one type, Option
  def direct1() {
    import scalaz._

    // Import the members of the type class instance for Option.
    import instance.Option.option.{join, bind}

    bind(o1)(x => if (x > 0) Some(2) else None)
    join(o2)
  }


  // Direct use of type class for multiple types
  def direct2() {
    import scalaz._

    // Import the type class instances for Option and List.
    import instance.Option.option
    import instance.List.list

    option.bind(o1)(x => if (x > 0) Some(2) else None)
    option.join(o2)
    list.join(l2)
  }
}
