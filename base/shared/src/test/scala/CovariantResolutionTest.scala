package scalaz

import scalaz.Scalaz._
import scalaz.zio.IO

class CovariantResolutionTest {
  implicitly[IsCovariant[IO[Int, ?]]]
}
