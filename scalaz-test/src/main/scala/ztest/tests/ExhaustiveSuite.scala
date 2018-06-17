package scalaz.test
package tests

import scala._

import scalaz.data.IList
import scalaz.Monad
import scalaz.Scalaz._
import property._

final class ExhaustiveSuite extends PureSuite {
  def doTests[G[_]: Monad](harness: Harness[() => ?, G]): G[Unit] = {
    import harness._
    section("exhaustiveS int range")(
      test {
        val ls = IList(1, 2, 3, 4, 5, 6)
        exhaustiveS(ls)(i => () => (i != 3)).map(
          errs =>
            if (errs === IList(true, true, false, true, true, true)) {
              Success()
            } else {
              Failure()
          }
        )
      }
    )
  }
}
