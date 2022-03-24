package scalaz.example

import scalaz.{ FreeAp, ~> , ValidationNel }
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.std.option._

import scala.reflect.ClassTag

// Example usage of free applicative
object FreeApUsage {

  // An algebra of primitive operations in parsing types from Map[String, Any]
  sealed trait ParseOp[A]
  case class ParseInt(key: String) extends ParseOp[Int]
  case class ParseString(key: String) extends ParseOp[String]
  case class ParseBool(key: String) extends ParseOp[Boolean]

  // Free applicative over Parse.
  def main(args: Array[String]): Unit = {
    type Parse[A] = FreeAp[ParseOp, A]

    // Smart constructors for Parse[A]
    def parseInt(key: String) = FreeAp.lift(ParseInt(key))
    def parseString(key: String) = FreeAp.lift(ParseString(key))
    def parseBool(key: String) = FreeAp.lift(ParseBool(key))

    def parseOpt[A: ClassTag](a: Any): Option[A] =
      a match {
        case a: A => Some(a)
        case _ => None
      }

    // Natural transformation to Option[A]
    def toOption(input: Map[String, Any]): ParseOp ~> Option =
      new (ParseOp ~> Option) {
        def apply[A](fa: ParseOp[A]) = fa match {
          case ParseInt(key) =>
            input.get(key).flatMap(parseOpt[java.lang.Integer](_).map(x => (x: Int)))
          case ParseString(key) => input.get(key).flatMap(parseOpt[String])
          case ParseBool(key) =>
            input.get(key).flatMap(parseOpt[java.lang.Boolean](_).map(x => (x: Boolean)))
        }
      }

    // Natural transformation to ValidationNel[String, A]
    type ValidatedParse[A] = ValidationNel[String, A]
    def toValidation(input: Map[String, Any]): ParseOp ~> ValidatedParse =
      new (ParseOp ~> ValidatedParse) {
        def apply[A](fa: ParseOp[A]) = fa match {
          case s@ParseInt(_) => toOption(input)(s)
                                     .toSuccessNel(s"${s.key} not found with type Int")
          case s@ParseString(_) => toOption(input)(s)
                                     .toSuccessNel(s"${s.key} not found with type String")
          case i@ParseBool(_) => toOption(input)(i)
                                  .toSuccessNel(s"${i.key} not found with type Boolean")
        }
      }

    // An example that returns a tuple of (String, Int, Boolean) parsed from Map[String, Any]
    val successfulProg: Parse[(String, Int, Boolean)] =
      (parseString("string") |@| parseInt("int") |@| parseBool("bool"))((_, _, _))

    // An example that returns a tuple of (Boolean, String, Int) parsed from Map[String, Any]
    val failedProg: Parse[(Boolean, String, Int)] =
      (parseBool("string") |@| parseString("list") |@| parseInt("bool"))((_, _, _))

    // Test input for programs
    val testInput: Map[String, Any] =
      Map("string" -> "foobar", "bool" -> true, "int" -> 4, "list" -> List(1, 2))

    // Run that baby
    println(successfulProg.foldMap(toOption(testInput)))
    println(successfulProg.foldMap(toValidation(testInput)))
    println(failedProg.foldMap(toOption(testInput)))
    println(failedProg.foldMap(toValidation(testInput)))
  }

}
