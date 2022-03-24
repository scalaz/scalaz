package scalaz.example

object DirectTypeClassUsage {

  val o1: Option[Int] = Some(0)
  val o2: Option[Option[Int]] = Some(Some(0))
  val l1: List[String] = List("one")
  val l2: List[List[String]] = List(List("one"))

  def main(args: Array[String]): Unit = {
    direct1()
    direct2()
  }

  // Direct use of type class for one type, Option
  def direct1(): Unit = {
    import scalaz._

    // Import the members of the type class instance for Option.
    import std.option.optionInstance.{join, bind}

    bind(o1)(x => if (x > 0) Some(2) else None)
    join(o2)
  }


  // Direct use of type class for multiple types
  def direct2(): Unit = {
    import scalaz._

    // Import the type class instances for Option and List.
    import std.option.{optionInstance, optionMonoid}
    import std.list.listInstance

    optionInstance.bind(o1)(x => if (x > 0) Some(2) else None)
    optionInstance.join(o2)
    listInstance.join(l2)

    implicit object IntSemigroup extends Semigroup[Int] {
      def append(f1: Int, f2: => Int): Int = f1 + f2
    }

    Semigroup[Option[Int]].append(Some(1), None)
  }

}
