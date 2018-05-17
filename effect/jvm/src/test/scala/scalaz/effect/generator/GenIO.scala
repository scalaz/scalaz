package scalaz.effect.generator

import org.scalacheck._
import scalaz.effect.{ ExitResult, IO, RTS }

trait GenIO {

  /**
   * Given a generator for `A`, produces a generator for `IO[E, A]` using the `IO.point` constructor.
   */
  def genSyncSuccess[E, A: Arbitrary]: Gen[IO[E, A]] = Arbitrary.arbitrary[A].map(IO.point[E, A](_))

  /**
   * Given a generator for `A`, produces a generator for `IO[E, A]` using the `IO.async` constructor.
   */
  def genAsyncSuccess[E, A: Arbitrary]: Gen[IO[E, A]] =
    Arbitrary.arbitrary[A].map(a => IO.async[E, A](cb => cb(ExitResult.Completed(a))))

  /**
   * Randomly uses either `genSyncSuccess` or `genAsyncSuccess` with equal probability.
   */
  def genSuccess[E, A: Arbitrary]: Gen[IO[E, A]] = Gen.oneOf(genSyncSuccess[E, A], genAsyncSuccess[E, A])

  /**
   * Given a generator for `E`, produces a generator for `IO[E, A]` using the `IO.fail` constructor.
   */
  def genSyncFailure[E: Arbitrary, A]: Gen[IO[E, A]] = Arbitrary.arbitrary[E].map(IO.fail[E, A])

  /**
   * Given a generator for `E`, produces a generator for `IO[E, A]` using the `IO.async` constructor.
   */
  def genAsyncFailure[E: Arbitrary, A]: Gen[IO[E, A]] =
    Arbitrary.arbitrary[E].map(err => IO.async[E, A](cb => cb(ExitResult.Failed(err))))

  /**
   * Randomly uses either `genSyncFailure` or `genAsyncFailure` with equal probability.
   */
  def genFailure[E: Arbitrary, A]: Gen[IO[E, A]] = Gen.oneOf(genSyncFailure[E, A], genAsyncFailure[E, A])

  /**
   * Randomly uses either `genSuccess` or `genFailure` with equal probability.
   */
  def genIO[E: Arbitrary: Cogen, A: Arbitrary: Cogen]: Gen[IO[E, A]] =
    Gen.oneOf(genSuccess[E, A], genFailure[E, A])

  /**
   * Given a generator for `IO[E, A]`, produces a sized generator for `IO[E, A]` which represents a transformation,
   * by using some random combination of the methods `map`, `flatMap`, `leftMap`, and any other method that does not change
   * the success/failure of the value, but may change the value itself.
   */
  def genLikeTrans[E: Arbitrary: Cogen, A: Arbitrary: Cogen](gen: Gen[IO[E, A]]): Gen[IO[E, A]] = {
    val functions: IO[E, A] => Gen[IO[E, A]] = io =>
      Gen.oneOf(
        genOfFlatMaps[E, A](io)(genSuccess),
        genOfMaps[E, A](io),
        genOfRace[E, A](io),
        genOfParallel[E, A](io)(genSuccess),
        genOfLeftMaps[E, A](io)
    )
    gen.flatMap(io => genTransformations(functions)(io))
  }

  /**
   * Given a generator for `IO[E, A]`, produces a sized generator for `IO[E, A]` which represents a transformation,
   * by using methods that can have no effect on the resulting value (e.g. `map(identity)`, `io.race(never)`, `io.par(io2).map(_._1)`).
   */
  def genIdentityTrans[E: Arbitrary: Cogen, A: Arbitrary: Cogen](gen: Gen[IO[E, A]]): Gen[IO[E, A]] = {
    val functions: IO[E, A] => Gen[IO[E, A]] = io =>
      Gen.oneOf(genOfIdentityFlatMaps[E, A](io),
                genOfIdentityMaps[E, A](io),
                genOfIdentityLeftMaps[E, A](io),
                genOfRace[E, A](io),
                genOfParallel[E, A](io)(genSuccess))
    gen.flatMap(io => genTransformations(functions)(io))
  }

  private def genTransformations[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
    functionGen: IO[E, A] => Gen[IO[E, A]]
  )(io: IO[E, A]): Gen[IO[E, A]] =
    Gen.sized { size =>
      def append1(n: Int, io: IO[E, A]): Gen[IO[E, A]] =
        if (n <= 0) io
        else
          (for {
            updatedIO <- functionGen(io)
          } yield updatedIO).flatMap(append1(n - 1, _))
      append1(size, io)
    }

  private def genOfMaps[E, A: Arbitrary: Cogen](io: IO[E, A]): Gen[IO[E, A]] =
    Arbitrary.arbitrary[A => A].map(f => io.map(f))

  private def genOfIdentityMaps[E, A](io: IO[E, A]): Gen[IO[E, A]] = Gen.const(io.map(identity))

  private def genOfLeftMaps[E: Arbitrary: Cogen, A](io: IO[E, A]): Gen[IO[E, A]] =
    Arbitrary.arbitrary[E => E].map(f => io.leftMap(f))

  private def genOfIdentityLeftMaps[E, A](io: IO[E, A]): Gen[IO[E, A]] =
    Gen.const(io.leftMap(identity))

  private def genOfFlatMaps[E, A: Cogen](io: IO[E, A])(
    gen: Gen[IO[E, A]]
  ): Gen[IO[E, A]] =
    gen.map(nextIO => io.flatMap(_ => nextIO))

  private def genOfIdentityFlatMaps[E, A](io: IO[E, A]): Gen[IO[E, A]] =
    Gen.const(io.flatMap(a => IO.point(a)))

  private def genOfRace[E, A](io: IO[E, A]): Gen[IO[E, A]] =
    Gen.const(io.race(IO.never))

  private def genOfParallel[E, A](io: IO[E, A])(gen: Gen[IO[E, A]]): Gen[IO[E, A]] =
    gen.map(parIo => io.par(parIo).map(_._1))

}
