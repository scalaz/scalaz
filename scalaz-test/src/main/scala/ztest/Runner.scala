package scalaz.test

import scala._, scala.Predef._
import java.lang.System

import scalaz.Void
import scalaz.data.{IList, Maybe2}
import IList.uncons
import scalaz.effect.{console, IO}
import scalaz.Scalaz._

object Runner {
  // Configuration.
  // Forwards-compatible by construction.
  final class Config private[Runner](_output: String => IO[Void, Unit]) {
    def withOutput(newOutput: String => IO[Void, Unit]) = new Config(newOutput)
    def output: String => IO[Void, Unit] = _output
  }

  val defaultConfig: Config = new Config(_output = console.putStrLn(_).attempt.void)

  private def printStrs[F[_]: Applicative](strs: IList[String], output: String => F[Unit]): F[Unit] = uncons(strs) match {
    case Maybe2.Just2(x, xs) => output(x) *> printStrs[F](xs, output)
    case _ => Applicative[F].pure(())
  }

  private def printStrss[F[_]: Applicative, E](strs: IList[IList[String]], output: String => F[Unit]): F[Unit] = uncons(strs) match {
    case Maybe2.Just2(x, xs) => printStrs(x, output) *> output("\n") *> printStrss(xs, output)
    case _ => Applicative[F].pure(())
  }

  def configured(suites: IList[() => Suite], config: Config): IO[Void, Boolean] = for {
    startTime <- IO.sync[Void, Long](System.currentTimeMillis)
    succeededSuites <- suites.traverse(suite =>
      suite().run.flatMap {
        case SuiteOutput(lines, allSucceeded) =>
          printStrs(lines, config.output).map(_ => allSucceeded)
      }
    )
    endTime <- IO.sync[Void, Long](System.currentTimeMillis)
    _ <- config.output(s"Testing took ${endTime - startTime} ms")
  } yield
    if (IList.isEmpty(succeededSuites)) false
    else succeededSuites.foldRight(true)(_ && _)

  def apply(suites: IList[() => Suite]): IO[Void, Boolean] =
    configured(suites, defaultConfig)

}
