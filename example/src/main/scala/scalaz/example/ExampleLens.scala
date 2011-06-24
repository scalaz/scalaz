package scalaz.example

import scalaz._, Scalaz._

object ExampleLens {
  def main(args: Array[String]) = run

  def run {
    case class Employee(name: String, salary: Int)
    implicit val EmployeeEqual = Equal.equalA[Employee]
    implicit val EmployeeShow = Show.showA[Employee]

    val salary: (Employee @@ Int) = lensG(_.salary, e => s => e copy (salary = s))
    val name: (Employee @@ String) = lensG(_.name, e => n => e copy (name = n))


    val giveRaise: Employee => Employee = salary mod (100 +)

    val tom = Employee("Tom", 4000)
    val dick = Employee("Dick", 3000)
    val harry = Employee("Harry", 5000)

    val higherTom = giveRaise(tom)
    higherTom assert_=== Employee("Tom", 4100)

    val modBoth = ((salary *** name) mod {
      case (s, n) => (s + 100, n + " Jones")
    })(harry, tom)
    modBoth assert_=== (Employee("Harry", 5100), Employee("Tom Jones", 4000))

    val modMonadically =
      for {
        _ <- salary += 100
        n <- name
        _ <- name := n + " Jones"
        e <- get
      } yield e
    val tomJones = modMonadically eval tom
    tomJones assert_=== Employee("Tom Jones", 4100)
  }
}
