package scalaz
package data

import scalaz.zio.{ ExitResult, Fiber, IO }
import scalaz.tc.{ Foldable, Unfoldable }

import scala.{ AnyVal, Nothing, Throwable, Unit }

object io {

  implicit class IOObjOps(private val ioObj: IO.type) extends AnyVal {

    final def absolvez[E, A](v: IO[E, E \/ A]): IO[E, A] =
      v.flatMap(fromDisjunction)

    final def fromDisjunction[E, A](v: E \/ A): IO[E, A] =
      v.fold(IO.fail, IO.now)

    final def fromMaybe[A](v: Maybe[A]): IO[Unit, A] =
      Maybe.fold(v)(IO.now, IO.fail(()))

    final def requirez[E, A](error: E): IO[E, Maybe[A]] => IO[E, A] =
      (io: IO[E, Maybe[A]]) =>
        io.flatMap { maybe =>
          Maybe.fold[A, IO[E, A]](maybe)(IO.now[A], IO.fail[E](error))
      }

    final def forkAllz[E, A, F[_]](
      as: F[IO[E, A]]
    )(implicit f: Foldable[F], u: Unfoldable[F]): IO[Nothing, Fiber[E, F[A]]] =
      f.foldRight(as, IO.point(Fiber.point[E, IList[A]](IList.empty))) { (aIO, asFiberIO) =>
          asFiberIO.par(aIO.fork).map {
            case (asFiber, aFiber) =>
              asFiber.zipWith(aFiber)((as, a) => a :: as)
          }
        }
        .map(
          fas =>
            fas.zipWith(Fiber.point(())) { (res, _) =>
              u.fromList(res)
          }
        )

    final def raceAllz[E, A, F[_]](io: IO[E, A], ios: F[IO[E, A]])(implicit f: Foldable[F]): IO[E, A] =
      f.foldLeft[IO[E, A], IO[E, A]](ios, io)(_ race _)

    final def reduceAllz[E, A, F[_]](a: IO[E, A], as: F[IO[E, A]])(f: (A, A) => A)(implicit fd: Foldable[F]): IO[E, A] =
      fd.foldLeft(as, a) { (l, r) =>
        l.par(r).map(f.tupled)
      }

    final def mergeAllz[E, A, B, F[_]](in: F[IO[E, A]])(zero: B, f: (B, A) => B)(implicit fd: Foldable[F]): IO[E, B] =
      fd.foldLeft[IO[E, A], IO[E, B]](in, IO.point[B](zero))((acc, a) => acc.par(a).map(f.tupled))

    final def terminate0z[F[_]](ts: F[Throwable])(implicit f: Foldable[F]): IO[Nothing, Nothing] =
      IO.terminate0(f.toList(ts))

    final def unsandboxz[E, A, F[_]](v: IO[F[Throwable] \/ E, A])(implicit f: Foldable[F]): IO[E, A] =
      v.catchAll[E, A] {
        case \/-(e)  => IO.fail(e)
        case -\/(ts) => IO.terminate0z(ts)
      }

  }

  implicit class IOOps[E, A](private val io: IO[E, A]) extends AnyVal {
    final def attemptz: IO[Nothing, E \/ A] =
      io.redeem[Nothing, E \/ A](e => IO.now(-\/(e)), a => IO.now(\/-(a)))

    final def raceBothz[E1 >: E, B](that: IO[E1, B]): IO[E1, A \/ B] =
      io.raceWith(that)((a, fiber) => fiber.interrupt.const(-\/(a)), (b, fiber) => fiber.interrupt.const(\/-(b)))

    final def or[E2, B](that: => IO[E2, B]): IO[E2, A \/ B] =
      io.redeem(_ => that.map(\/-(_)), e => IO.now(-\/(e)))

    final def sandboxedz[F[_]: Foldable](implicit u: Unfoldable[F]): IO[\/[F[Throwable], E], A] =
      io.run.flatMap[\/[F[Throwable], E], A](_ match {
        case ExitResult.Completed(value) =>
          IO.now(value)
        case ExitResult.Failed(error, _) =>
          IO.fail(\/-[F[Throwable], E](error))
        case ExitResult.Terminated(ts) =>
          IO.fail(-\/[F[Throwable], E](u.fromList(IList(ts: _*))))
      })

    final def sandboxWithz[E2, B, F[_]: Foldable](
      f: IO[\/[F[Throwable], E], A] => IO[\/[F[Throwable], E2], B]
    )(implicit u: Unfoldable[F]): IO[E2, B] =
      f(io.sandboxedz).catchAll[E2, B] {
        case \/-(e)  => IO.fail(e)
        case -\/(ts) => IO.terminate0z(ts)
      }

  }

}
