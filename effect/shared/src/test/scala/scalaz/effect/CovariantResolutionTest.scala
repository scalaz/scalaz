package scalaz.effect

import scalaz.IsCovariant

class CovariantResolutionTest {
  implicitly[IsCovariant[IO[Int, ?]]]
}
