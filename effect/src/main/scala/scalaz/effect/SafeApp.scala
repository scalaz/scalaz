package scalaz
package effect

import IO._

/**
 * A safe alternative to the `App` trait in the Scala standard library. This
 * trait provides an implementation of the `main` method by calling
 * `unsafePerformIO` on a specified `IO` action.
 */
trait SafeApp {

  def run(args: ImmutableArray[String]): IO[Unit] = runl(args.toList)

  def runl(args: List[String]): IO[Unit] = runc

  def runc: IO[Unit] = ioUnit

  final def main(args: Array[String]) {
    run(ImmutableArray.fromArray(args)).unsafePerformIO()
  }

}
