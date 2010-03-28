package scalaz.example

import scalaz._

/**
 * Demonstrates how to write an assertion method that depends on Equal,
 * how to provide Equal instances for your own types, and the many static
 * errors you can catch when comparing apples and oranges.
 */
object ExampleEqual {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    class Fruit
    case class Apple() extends Fruit
    case class Orange() extends Fruit

    class Vehicle

    def checkEqual[X: Equal](o1: X, o2: X): Boolean = o1 ≟ o2

    // Case class equality is suitable, so expose it as a implicit instance of scalaz.Equal
    // for Apple and Orange.
    implicit def dogEqual = equalA[Apple]
    implicit def catEqual = equalA[Orange]

    implicit def vehicleToEqual = equalA[Vehicle]

    assert(new Apple ≟ new Apple)
    checkEqual(new Apple, new Apple)

    assert(new Orange ≟ new Orange)
    checkEqual(new Orange, new Orange)

    //assert(new Fruit ≠ new Orange) // does not compile
    //checkEqual(new Fruit, new Orange) // does not compile

    //assert(new Orange ≠ new Fruit) // does not compile
    //checkEqual(new Orange, new Fruit) // does not compile

    //assert(new Apple ≠ new Orange) //does not compile
    //checkEqual(new Apple, new Orange) //does not compile

    //assert(new Apple ≠ new Vehicle) //does not compile
    //checkEqual(new Apple, new Vehicle) //does not compile

    //assert(new Fruit ≠ "") //does not compile
    //checkEqual(new Fruit, "") //does not compile

    assert(List(new Orange) ≟ List(new Orange))

    //assert(List(new Orange) ≟ List(new Apple)) //does not compile  
  }
}