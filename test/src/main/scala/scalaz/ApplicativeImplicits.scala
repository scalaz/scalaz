package scalaz

import org.scalacheck.{Gen, Arbitrary}
import scalaz.Applicative._

trait ApplicativeImplicits extends PureImplicits with ApplyImplicits with FunctorImplicits {
  implicit val GenApplicative = applicative[Gen]

  implicit val ArbitraryApplicative = applicative[Arbitrary]
}
