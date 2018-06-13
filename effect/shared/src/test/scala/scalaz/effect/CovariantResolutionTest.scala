package scalaz
package effect

import scalaz.zio.IO

class CovariantResolutionTest extends IOInstances {
  implicitly[IsCovariant[IO[Int, ?]]]
}
