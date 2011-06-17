package scalaz.example
  /* todo
import scalaz._
import Scalaz._

object ExampleLens {
  case class Employee(name: String, salary: Int)
  val salary: Lens[Employee, Int] = Lens(_.salary, (e, s) => e copy (salary = s))
  val name: Lens[Employee, String] = Lens(_.name, (e, n) => e copy (name = n))

  val giveRaise: Employee => Employee = salary mod (_, _ + 100)

  val tom = Employee("Tom", 4000)
  val dick = Employee("Dick", 3000)
  val harry = Employee("Harry", 5000)

  val higherTom = giveRaise(tom) // Employee("Tom", 4100)

  val modBoth = (salary *** name) mod ((harry, tom), {
    case (s, n) => (s + 100, n + " Jones")
  }) // (Employee("Harry", 5100), Employee("Tom Jones", 4000))  

  val modMonadically = for {
    _ <- salary += 100
    n <- name
    _ <- name := n + " Jones"
    e <- init
  } yield e

  val tomJones = modMonadically ! tom // Employee("Tom Jones", 4100)
  
}
             */