package scalaz

import scalaz.Tags._
import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.reducer

object ReducerTest extends SpecLite {
  import EndoTest.endoIntEqual

  implicit val intDualEqual: Equal[Int @@ Dual] =
    Equal.equalBy(Tag.unwrap)

  checkAll(reducer.laws[Int, List[Int]])
  checkAll(reducer.laws[Int, IList[Int]])
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
}
