package scalaz.example

import scalaz.Semigroup


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
    import instance.Option.{option, optionMonoid}
    import instance.List.list

    option.bind(o1)(x => if (x > 0) Some(2) else None)
    option.join(o2)
    list.join(l2)

    implicit object IntSemigroup extends Semigroup[Int] {
      def append(f1: Int, f2: => Int): Int = f1 + f2
    }

    Semigroup[Option[Int]].append(Some(1), None)
  }

}
