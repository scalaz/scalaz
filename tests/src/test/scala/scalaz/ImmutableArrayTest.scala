package scalaz
import org.scalacheck.Prop.forAll

object ImmutableArrayTest extends SpecLite {

  "Issue #525" in {
    val xs = ImmutableArray.fromArray(Array(1)) ++ ImmutableArray.fromArray(Array("a"))
    xs.toArray.toList must_==(Array(1, "a").toList)
  }

}
