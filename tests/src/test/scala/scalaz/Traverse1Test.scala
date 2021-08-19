package scalaz

import std.AllInstances._
import syntax.traverse1._

object Traverse1Test extends SpecLite {

  "traverse1M" in {
    val x = NonEmptyList(IList(1, 2, 3, 4), IList(3, 4, 5, 6)).traverse1M(_.groupBy(_ % 3))
    x must_=== IMap(
      0 -> NonEmptyList(3, 3, 6),
      1 -> NonEmptyList(1, 4, 4),
      2 -> NonEmptyList(2, 5)
    )
  }

  "sequence1M" in {
    val x = NonEmptyList(IList(1, 2, 3, 4), IList(3, 4, 5, 6)).map(_.groupBy(_ % 3)).sequence1M
    x must_=== IMap(
      0 -> NonEmptyList(3, 3, 6),
      1 -> NonEmptyList(1, 4, 4),
      2 -> NonEmptyList(2, 5)
    )
  }

}
