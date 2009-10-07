package scalaz

import org.scalacheck.{Gen, Arbitrary}
import scalaz.Monad._

trait MonadImplicits extends BindImplicits with PureImplicits {
  implicit val GenMonad = monad[Gen]

  implicit val ArbitraryMonad = monad[Arbitrary]
}
