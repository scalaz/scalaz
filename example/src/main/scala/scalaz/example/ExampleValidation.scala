package scalaz.example

import scalaz._

import collection.immutable.List
import collection.Traversable

object ExampleValidation {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Constructing Validations
    failure[String, Int]("error") assert_≟ "error".fail[Int]
    success[String, Int](0) assert_≟ 0.success[String]
    validation[String, Int](Left("error")) assert_≟ "error".fail[Int]
    validation[String, Int](Right(0)) assert_≟ 0.success[String]

    // Extracting success or failure values
    val s: Validation[String, Int] = 1.success
    val f: Validation[String, Int] = "error".fail
    s.toOption assert_≟ some(1)
    s.fail.toOption assert_≟ none[String]
    f.toOption assert_≟ none[Int]
    f.fail.toOption assert_≟ some("error")

    // It is recommended to use fold rather than pattern matching:
    val result: String = s.fold(e => "got error: " + e, s => "got success: " + s.toString)

    s match {
      case Success(a) => "success"
      case Failure(e) => "fail"
    }

    // Validation#| is analogous to Option#getOrElse
    (f | 1) assert_≟ 1

    // Validation is a Monad, and can be used in for comprehensions.
    val k1 = for {
      i <- s
      j <- s
    } yield i + j
    k1.toOption assert_≟ Some(2)

    // The first failing sub-computation fails the entire computation.
    val k2 = for {
      i <- f
      j <- f
    } yield i + j
    k2.fail.toOption assert_≟ Some("error")

    // Validation is also an Applicative Functor, if the type of the error side of the validation is a Semigroup.
    // A number of computations are tried. If the all success, a function can combine them into a Success. If any
    // of them fails, the individual errors are accumulated.

    // Combining validation errors using the String Semigroup.
    val k3 = (f <**> f){ _ + _ }
    k3.fail.toOption assert_≟ some("errorerror")

    // The String semigroup wasn't particularly useful. A better candidate is NonEmptyList. Below, we use
    // Validation#liftFailNel to convert from Validation[String, Int] to Validation[NonEmptyList[String], Int].
    // The type alias ValidationNEL makes this more concise.
    val fNel: ValidationNEL[String, Int] = f.liftFailNel

    // Use the NonEmptyList semigroup to accumulate errors using the Validation Applicative Functor.
    val k4 = (fNel <**> fNel){ _ + _ }
    k4.fail.toOption assert_≟ some(nel("error", "error"))

    person
    parseNumbers
  }

  /**
   * See Automated Validation with Applicatives and Semigroups <a href="http://blog.tmorris.net/automated-validation-with-applicatives-and-semigroups-for-sanjiv/">Part 1</a>
   * and <a href="http://blog.tmorris.net/automated-validation-with-applicatives-and-semigroups-part-2-java/">Part 2</a>
   */
  def person {
    sealed trait Name extends NewType[String]
    object Name {
      def apply(s: String): Validation[String, Name] = if (s.headOption.exists(_.isUpper))
        (new Name {val value = s}).success
      else
        "Name must start with a capital letter".fail
    }

    sealed trait Age extends NewType[Int]
    object Age {
      def apply(a: Int): Validation[String, Age] = if (0 to 130 contains a)
        (new Age {val value = a}).success
      else
        "Age must be in range".fail
    }

    case class Person(name: Name, age: Age)
    def mkPerson(name: String, age: Int) = (Name(name).liftFailNel ⊛ Age(age).liftFailNel){ (n, a) => Person(n, a)}

    mkPerson("Bob", 31).isSuccess assert_≟ true
    mkPerson("bob", 131).fail.toOption assert_≟ some(nel("Name must start with a capital letter", "Age must be in range"))
  }

  def parseNumbers {
    def only[A](as: Traversable[A]): Validation[String, A] = {
      val firstTwo = as.take(2).toSeq
      validation((firstTwo.size != 1) either "required exactly one element" or firstTwo.head)
    }

    def empty[A](as: Traversable[A]): Validation[String, Unit] =
      validation(!as.isEmpty either "expected an empty collection" or ())

    // Combine two validations with the Validation Applicative Functor, using only the success
    // values from the first.
    val x: ValidationNEL[String, Int] = only(Seq(1)).liftFailNel <* empty(Seq.empty).liftFailNel
    x assert_≟ 1.successNel[String]

    val badInput = """42
            |aasf
            |314
            |xxx""".stripMargin
    parse(badInput) assert_≟ nel("java.lang.NumberFormatException: For input string: \"aasf\"",
      "java.lang.NumberFormatException: For input string: \"xxx\"").fail[List[Int]]
    val validInput = """42
            |314""".stripMargin
    parse(validInput) assert_≟ List(42, 314).successNel[String]
  }

  /**
   * Parse text containing a list of integers, each on a separate line.
   */
  def parse(text: String): ValidationNEL[String, List[Int]] = {
    val lines = text.lines.toList
    def parseInt(s: String): ValidationNEL[String, Int] = {
      val projection: FailProjection[String, Int] = s.parseInt.fail ∘ (_.toString)
      // todo this can't be inferred if Pure is invariant. Why not? 
      projection.lift[NonEmptyList, String]
    }
    val listVals: List[ValidationNEL[String, Int]] = lines.map(parseInt(_))
    // Sequence the List using the Validation Applicative Functor.
    listVals.sequence[({type λ[α]=ValidationNEL[String, α]})#λ, Int]
  }
}