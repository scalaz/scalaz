package scalaz
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.anyVal._

object ImmutableArrayTest extends SpecLite {

  "Issue #525" in {
    val xs = ImmutableArray.fromArray(Array(1)) ++ ImmutableArray.fromArray(Array("a"))
    xs.toArray.toList must_==(Array(1, "a").toList)
  }

  checkAll(equal.laws[ImmutableArray[Int]])
  checkAll(foldable.laws[ImmutableArray])

}
