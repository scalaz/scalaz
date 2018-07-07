package scalaz

import scalaz.Scalaz._
import scalaz.zio.IO

import Predef._
import prop.IsCovariant

class CovariantResolutionTest {
  implicitly[IsCovariant[IO[Int, ?]]]
}
