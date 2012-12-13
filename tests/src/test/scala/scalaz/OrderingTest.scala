package scalaz

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._

class OrderingTest extends testlib.Spec {
  checkAll("Ordering", enum.laws[Ordering])
}
