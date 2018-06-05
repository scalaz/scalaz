package scalaz
package effect

class CovariantResolutionTest {
  implicitly[IsCovariant[IO[Int, ?]]]
}
