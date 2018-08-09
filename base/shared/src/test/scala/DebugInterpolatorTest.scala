package scalaz

import Predef._
import data.IList
import Scalaz._

object DebugInterpolatorTest {

  // TODO: link up testz and make this a real test

  z"before ${1} after"
  z"before ${"foo"} after"
  z"before ${IList(1, 2, 3)} after"

}
