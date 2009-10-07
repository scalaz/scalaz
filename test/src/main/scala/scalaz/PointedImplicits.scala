package scalaz

import org.scalacheck.{Gen, Arbitrary}
import Scalaz._
import scalaz.Pointed._

trait PointedImplicits extends FunctorImplicits with PureImplicits {
  implicit val GenPointed = pointed[Gen]

  implicit val ArbitraryPointed = pointed[Arbitrary]
}
