// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

/**
 * The entry point for a purely-functional application on the JVM.
 *
 * {{{
 * import scalaz.effect.{IO, SafeApp}
 * import scalaz.effect.console._
 *
 * object MyApp extends SafeApp {
 *   def run(args: List[String]): IO[E, Unit] =
 *     for {
 *       _ <- putStrLn("Hello! What is your name?")
 *       n <- getStrLn
 *       _ <- putStrLn("Hello, " + n + ", good to meet you!")
 *     } yield ()
 * }
 * }}}
 */
trait SafeApp extends RTS {
  /**
   * The main function of the application, which will be passed the command-line
   * arguments to the program.
   */
  def run[E](args: List[String]): IO[E, Unit]

  /**
   * The Scala main function, intended to be called only by the Scala runtime.
   */
  final def main(args0: Array[String]): Unit = unsafePerformIO(run(args0.toList))
}
