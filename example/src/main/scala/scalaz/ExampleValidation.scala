package scalaz

import collection.immutable.List
import collection.Traversable

object ExampleValidation {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    def only[A](as: Traversable[A]): Validation[String, A] = {
      val firstTwo = as.take(2).toSeq
      validation((firstTwo.size != 1) either "required exactly one element" or firstTwo.head)
    }

    def empty[A](as: Traversable[A]): Validation[String, Unit] =
      validation(!as.isEmpty either "expected an empty collection" or ())

    // Combine two validations with the Validation Applicative Functor, using only the success
    // values from the first.
    val x: ValidationNEL[String, Int] = only(Seq(1)).fail.liftNel <* empty(Seq.empty).fail.liftNel
    x assert_≟ 1.successNel[String]

    val badInput = """42
            |aasf
            |314
            |xxx""".stripMargin
    parse(badInput) assert_≟ nel1("java.lang.NumberFormatException: For input string: \"aasf\"",
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
    def parseInt(s: String): ValidationNEL[String, Int] = (s.parseInt.fail ∘ (_.toString)).lift
    val listVals: List[ValidationNEL[String, Int]] = lines.map(parseInt(_))
    // Sequence the List using the Validation Applicative Functor.
    listVals.sequence[PartialApply1Of2[ValidationNEL, String]#Apply, Int]
  }
}