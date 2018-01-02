package scalaz.effect

trait SafeApp {
  /**
   * The main function of the application, which will be passed the command-line
   * arguments to the program.
   */
  def run[E](args: List[String]): IO[E, Unit]
}
