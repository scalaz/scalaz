package scalaz.effect

import scala.scalajs.js

trait SafeApp extends js.JSApp {
  /**
   * The main function of the Scala.js application.
   */
  def run: IO[Unit]

  final def main(): Unit = ???
}
