package scalaz

import org.scalacheck.{Gen, Arbitrary}
import Scalaz._
import Apply._

trait ApplyImplicits extends PureImplicits with FunctorImplicits with BindImplicits {
  implicit val GenApply = FunctorBindApply[Gen]

  implicit val ArbitraryApply = FunctorBindApply[Arbitrary]
}
