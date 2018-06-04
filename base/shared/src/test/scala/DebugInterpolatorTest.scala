package scalaz

object DebugInterpolatorTest {

  // TODO: link up ztest and make this a real test

  import Scalaz._, data._

  z"before ${1} after"
  z"before ${"foo"} after"
  z"before ${IList(1, 2, 3)} after"

}
