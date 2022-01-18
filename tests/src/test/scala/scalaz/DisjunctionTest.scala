package scalaz

import std.AllInstances._
import Maybe._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object DisjunctionTest extends SpecLite {

  checkAll(order.laws[Int \/ Int])
  checkAll(monoid.laws[Int \/ Int])
  checkAll(bindRec.laws[\/[Int, *]])
  checkAll(monad.laws[\/[Int, *]])
  checkAll(monadError.laws[\/[Int, *], Int])
  checkAll(plus.laws[\/[Int, *]])
  checkAll(traverse.laws[\/[Int, *]])
  checkAll(bitraverse.laws[\/])
  checkAll(associative.laws[\/])
  checkAll(band.laws[ISet[Int] \/ MaxMaybe[Int]])

  "fromTryCatchThrowable" in {
    class Foo extends Throwable
    final class Bar extends Foo
    val foo = new Foo
    val bar = new Bar

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]

    \/.fromTryCatchThrowable[Int, Foo](1) must_=== \/.right(1)
    \/.fromTryCatchThrowable[Int, Foo](throw foo) must_=== \/.left(foo)
    \/.fromTryCatchThrowable[Int, Foo](throw bar) must_=== \/.left(bar)
    \/.fromTryCatchThrowable[Int, Bar](throw foo).mustThrowA[Foo]
  }

  "attempt" in {
    import scalaz.syntax.either._

    // the JVM number parsers give useless error messages. Prefer .parseInt and
    // friends from scalaz.std.syntax.string, giving hand-crafted error messages
    // as of scalaz 7.3.0.
    {
      for {
        s <- "foo".right[String]
        i <- \/.attempt(s.toInt)(_.getMessage + " not an integer")
      } yield i
    } must_=== \/.left("For input string: \"foo\" not an integer")

    \/.attempt("1".toInt)(_.getMessage) must_=== \/.right(1)
  }

  "recover" in {
    sealed trait Foo
    case object Bar extends Foo
    case object Baz extends Foo

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]

    -\/[Foo, Int](Bar).recover({ case Bar => 1 }) must_=== \/-(1)
    -\/[Foo, Int](Bar).recover({ case Baz => 1 }) must_=== -\/(Bar)
    \/.right[Foo, Int](1).recover({ case Bar => 4 }) must_=== \/-(1)
  }

  "recoverWith" in {
    sealed trait Foo
    case object Bar extends Foo
    case object Baz extends Foo

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]

    val barToBaz: PartialFunction[Foo, \/[Foo, Int]] = {
      case Bar => -\/(Baz)
    }

    val bazToInt: PartialFunction[Foo, \/[Foo, Int]] = {
      case Baz => \/-(1)
    }

    -\/[Foo, Int](Bar).recoverWith(barToBaz) must_=== -\/(Baz)
    -\/[Foo, Int](Bar).recoverWith(bazToInt) must_=== -\/(Bar)
    \/.right[Foo, Int](1).recoverWith(barToBaz) must_=== \/-(1)
  }

  "validation" in {
    import syntax.either._
    import syntax.validation._

    3.right[String].validation must_=== 3.success[String]
    "Hello".left[Int].validation must_=== "Hello".failure[Int]
  }

  "validationNel" in {
    import syntax.either._
    import syntax.validation._
    import syntax.apply._

    3.right[String].validationNel must_=== 3.successNel[String]
    ("hello".left[Int].validationNel |@| "world".left[Int].validationNel).tupled must_===
      ("hello".failureNel[Int] |@| "world".failureNel[Int]).tupled
  }

  "lifted Reducer is short-circuiting" in {
    ApplyTest.unfoldrOptShortCircuiting[\/[Int, *]](-\/(42))
  }

  object instances {
    def semigroup[A: Semigroup, B: Semigroup] = Semigroup[A \/ B]
    def monoid[A: Semigroup, B: Monoid] = Monoid[A \/ B]
    def band[A: Band, B: Band] = Band[A \/ B]

    // checking absence of ambiguity
    def semigroup[A: Semigroup, B: Monoid] = Semigroup[A \/ B]
    def semigroup[A: Band, B: Band: Monoid] = Semigroup[A \/ B]
  }
}
