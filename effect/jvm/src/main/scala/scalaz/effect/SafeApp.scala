// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import scala.concurrent.duration.Duration

/**
 * The entry point for a purely-functional application on the JVM.
 *
 * {{{
 * import java.io.IOException
 * import scalaz.effect.{IO, SafeApp}
 * import scalaz.effect.console._
 *
 * object MyApp extends SafeApp {
 *
 *   def run(args: List[String]): IO[Void, ExitStatus] =
 *     myAppLogic.attempt.map(_.fold(_ => 1)(_ => 0)).map(ExitStatus.ExitNow(_))
 *
 *   def myAppLogic: IO[IOException, Unit] =
 *     for {
 *       _ <- putStrLn("Hello! What is your name?")
 *       n <- getStrLn
 *       _ <- putStrLn("Hello, " + n + ", good to meet you!")
 *     } yield ()
 * }
 * }}}
 */
trait SafeApp extends RTS {

  sealed trait ExitStatus
  object ExitStatus {
    case class ExitNow(code: Int)                         extends ExitStatus
    case class ExitWhenDone(code: Int, timeout: Duration) extends ExitStatus
    case object DoNotExit                                 extends ExitStatus
  }

  /**
   * The main function of the application, which will be passed the command-line
   * arguments to the program and has to return an `IO` with the errors fully handled.
   */
  def run(args: List[String]): IO[Void, ExitStatus]

  /**
   * The Scala main function, intended to be called only by the Scala runtime.
   */
  final def main(args0: Array[String]): Unit =
    unsafePerformIO(run(args0.toList)) match {
      case ExitStatus.ExitNow(code) =>
        sys.exit(code)
      case ExitStatus.ExitWhenDone(code, timeout) =>
        unsafeShutdownAndWait(timeout)
        sys.exit(code)
      case ExitStatus.DoNotExit =>
    }

}
