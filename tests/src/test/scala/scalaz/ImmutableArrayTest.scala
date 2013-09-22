package scalaz

class ImmutableArrayTest extends Spec {

  "Issue #525" in {
    val xs = ImmutableArray.fromArray(Array(1)) ++ ImmutableArray.fromArray(Array("a"))
    xs.toArray must_==(Array(1, "a"))
  }

}
