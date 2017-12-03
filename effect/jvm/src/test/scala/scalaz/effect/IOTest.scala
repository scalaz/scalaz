package scalaz.effect

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.concurrent.ExecutionEnv
import org.specs2.specification.AroundTimeout

import scalaz.data.Disjunction.{-\/, \/-}

class IOTest(implicit ee: ExecutionEnv) extends Specification
  with AroundTimeout
  with ScalaCheck
  with RTS {

  override val defaultHandler = t => IO.unit

  def is = {
    s2"""
    IO properties
      success is independent of construction                     $testSuccessfulIOConstruction
      failure is independent of construction                     $testFailedIOConstruction
      absolving attempts is identity                             $testAbsolveAttemptIsIdentity
    """
  }

  def testSuccessfulIOConstruction: Prop = {
    Prop.forAll(generators.primitives.successfulIO[Unit], generators.primitives.successfulIO[Unit]) {
      (a, b) => {
        tryUnsafePerformIO(a) must_=== tryUnsafePerformIO(b)
      }
    }
  }

  def testFailedIOConstruction: Prop = {
    Prop.forAll { (t: Throwable) =>
      implicit val arbitraryT = Arbitrary(Gen.const(t))
      Prop.forAll(generators.primitives.failedIO[Unit], generators.primitives.failedIO[Unit]) {
        (a, b) => {
          tryUnsafePerformIO(a) must_=== tryUnsafePerformIO(b)
        }
      }
    }

  }

  def testAbsolveAttemptIsIdentity: Prop = {
    Prop.forAll(generators.io[Unit]) { io =>
      tryUnsafePerformIO(IO.absolve(io.attempt)) must_=== tryUnsafePerformIO(io)
    }
  }

  object generators {

    object primitives {

      // Pure generators

      def now[A](implicit arbitrary: Arbitrary[A]): Gen[IO[A]] = {
        arbitrary.arbitrary.map(IO.now).label("now")
      }

      def point[A](implicit arbitrary: Arbitrary[A]): Gen[IO[A]] = {
        arbitrary.arbitrary.map(IO.point(_)).label("point")
      }

      def fail[A](implicit arbitrary: Arbitrary[Throwable]): Gen[IO[A]] = {
        arbitrary.arbitrary.map[IO[A]](IO.fail).label("fail")
      }

      // Synchronous generators

      def successfulSync[A](implicit arbitrary: Arbitrary[A]): Gen[IO[A]] = {
        arbitrary.arbitrary.map(IO.sync(_)).label("successfulSync")
      }

      def failedSync[A](implicit arbitrary: Arbitrary[Throwable]): Gen[IO[A]] = {
        arbitrary.arbitrary.map[IO[A]](t => IO.sync(throw t)).label("failedSync")
      }

      // Asynchronous generators

      def successfulAsyncNow[A](implicit arbitrary: Arbitrary[A]): Gen[IO[A]] = {
        arbitrary.arbitrary.map { a =>
          IO.async0[A] { _ =>
            AsyncReturn.now(a)
          }
        }.label("successfulAsyncNow")
      }

      def successfulAsyncLater[A](implicit arbitrary: Arbitrary[A]): Gen[IO[A]] = {
        arbitrary.arbitrary.map[IO[A]] { a =>
          IO.async0 { callback =>
            callback(\/-(a))
            AsyncReturn.later
          }
        }.label("successfulAsyncLater")
      }

      def failedAsync[A](implicit arbitrary: Arbitrary[Throwable]): Gen[IO[A]] = {
        arbitrary.arbitrary.map[IO[A]] { t =>
          IO.async0 { callback =>
            callback(-\/(t))
            AsyncReturn.later
          }
        }.label("failedAsync")
      }

      // Wrapping up

      def successfulIO[A](implicit a: Arbitrary[A]): Gen[IO[A]] = {
        Gen.oneOf(
          now[A],
          point[A],
          successfulSync[A],
          successfulAsyncNow[A],
          successfulAsyncLater[A],
        )
      }

      def failedIO[A](implicit t: Arbitrary[Throwable]): Gen[IO[A]] = {
        Gen.oneOf(
          fail[A],
          failedSync[A],
          failedAsync[A]
        )
      }
    }

    object composites {
      def bracket[A](implicit a: Arbitrary[A], t: Arbitrary[Throwable]): Gen[IO[A]] = (for {
        acquire <- io[A]
        release <- io[Unit]
        use <- io[A]
      } yield {
        acquire.bracket[A](_ => release)(_ => use)
      }).label("bracket")
    }

    // wrapping up

    import composites._
    import primitives._

    def io[A](implicit a: Arbitrary[A], t: Arbitrary[Throwable]): Gen[IO[A]] = {
      Gen.lzy(Gen.frequency(
        5 -> successfulIO[A],
        3 -> failedIO[A],
        1 -> bracket[A]
      ))
    }

  }


}
