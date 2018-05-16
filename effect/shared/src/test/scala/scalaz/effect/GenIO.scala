package scalaz.effect

import org.scalacheck.Gen

trait GenIO {

  def genSyncSuccess[E, A](genA: Gen[A]): Gen[IO[E, A]] = for (a <- genA) yield IO.point(a)

  def genAsyncSuccess[E, A](genA: Gen[A]): Gen[IO[E, A]] =
    for (a <- genA) yield IO.async[E, A](_(ExitResult.Completed(a)))

  def genSuccess[E, A](genA: Gen[A]): Gen[IO[E, A]] =
    for {
      syncA  <- genSyncSuccess(genA)
      asyncA <- genAsyncSuccess(genA)
    } yield Gen.oneOf(syncA, asyncA)

  def genSyncFailure[E, A](genE: Gen[E]): Gen[IO[E, A]] = for (e <- genE) yield IO.fail(e)

  def genAsyncFailure[E, A](genE: Gen[E]): Gen[IO[E, A]] =
    for (e <- genE) yield IO.async[E, A](_(ExitResult.Failed(e)))

  def genFailure[E, A](genE: Gen[E]): Gen[IO[E, A]] =
    for {
      syncE  <- genSyncFailure(genE)
      asyncE <- genAsyncFailure(genE)
    } yield Gen.oneOf(syncE, asyncE)

  def genUnLikeTrans[E, A](gen: Gen[IO[E, A]], genE: Gen[E], genA: Gen[A]): Gen[IO[E, A]] =
    for {
      e  <- genE
      a  <- genA
      g  <- gen
      m  = g.map(_ => a)
      fm = g.flatMap(_ => IO.fail(e))
    } yield Gen.oneOf(m, fm, m <* fm, m *> fm)

  // def genLikeTrans[E, A](gen: Gen[IO[E, A]], genE: Gen[E], genA: Gen[A]): Gen[IO[E, A]] =
  //  for {
  //    e  <- genE
  //    a  <- genA
  //    g  <- gen
  //    m  = g.map(_ => a)
  //    fm = g.orElse(IO.fail(e))
  //  } yield Gen.oneOf(m, fm, m <* fm, m *> fm)
}
