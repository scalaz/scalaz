package scalaz
package scalacheck

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

/**
 * Instances of {@link scalacheck.Arbitrary} for JVM-only parts of Scalaz.
 */
abstract class ScalazArbitraryPlatform extends ScalazArbitrary1 {

  import scalaz.concurrent.Future
  implicit def FutureArbitrary[A: Arbitrary]: Arbitrary[Future[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => Future.now(x)))

  import scalaz.concurrent.Task
  implicit def TaskArbitrary[A: Arbitrary]: Arbitrary[Task[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => Task.now(x)))
}
