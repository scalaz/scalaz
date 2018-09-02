package scalaz

import scalaz.Tags._
import scalaz.std.AllInstances._
import scalaz.syntax.contravariant._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.reducer

object ReducerTest extends SpecLite {
  import EndoTest.endoIntEqual

  implicit val intDualEqual: Equal[Int @@ Dual] =
    Equal.equalBy(Tag.unwrap)

  checkAll(reducer.laws[Int, List[Int]])
  checkAll(reducer.laws[Int, IList[Int]])
  checkAll(reducer.laws[Int, NonEmptyList[Int]])
  checkAll(reducer.laws[Int, Stream[Int]])
  checkAll(reducer.laws[Int, Vector[Int]])
  checkAll(reducer.laws[Boolean, Boolean])
  checkAll(reducer.laws[Boolean, Boolean @@ Conjunction])
  checkAll(reducer.laws[Int => Int, Endo[Int]])
  checkAll(reducer.laws[Int, Int @@ Dual])
  checkAll(reducer.laws[Int, Int @@ Multiplication])
  checkAll(reducer.laws[BigInt, BigInt @@ Multiplication])
  checkAll(reducer.laws[Int, Option[Int] @@ First])
  checkAll(reducer.laws[Option[Int], Option[Int] @@ First])
  checkAll(reducer.laws[Int, Option[Int] @@ Last])
  checkAll(reducer.laws[Option[Int], Option[Int] @@ Last])


  "Int multiplication should terminate when encountering 0" in {
    val R = implicitly[Reducer[Int, Int @@ Multiplication]]
    implicit val S: Show[Int @@ Multiplication] = Show[Int].contramap(Tag.unwrap)

    val f: Int => Maybe[(Int, Int)] = i => {
      if(i >= 0) Maybe.just((i-1, i))
      else sys.error("BOOM!")
    }
    val g = (i: Int) => f(i) map (_.swap)

    R.unfoldl(5)(f) must_=== Multiplication(0)
    R.unfoldr(5)(g) must_=== Multiplication(0)
  }

  "conjunction should terminate when encountering false" in {
    val R = implicitly[Reducer[Boolean, Boolean @@ Conjunction]]
    implicit val S: Show[Boolean @@ Conjunction] = Show[Boolean].contramap(Tag.unwrap)

    val f: Int => Maybe[(Int, Boolean)] = i => {
      if(i > 0) Maybe.just((i-1, true))
      else if(i == 0) Maybe.just((i-1, false))
      else sys.error("BOOM!")
    }
    val g = (i: Int) => f(i) map (_.swap)

    R.unfoldl(5)(f) must_=== Conjunction(false)
    R.unfoldr(5)(g) must_=== Conjunction(false)
  }

}
