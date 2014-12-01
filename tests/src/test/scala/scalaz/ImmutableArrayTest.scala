package scalaz

object ImmutableArrayTest extends SpecLite {

  "Issue #812" in {
    val xs = ImmutableArray.fromArray(Array("test"))
    val t = xs.tail
    t.toArray.toList must_== Array[String]().toList
  }

}
