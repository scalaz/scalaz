package scalaz
package concurrent


import scalaz.concurrent.MVar.newEmptyMVar
import scalaz.effect.IO

object MVarTest extends SpecLite {

  def forkIO(f: => IO[Unit])(implicit s: Strategy): IO[Unit] =
    IO { s(f.unsafePerformIO); () }

  /** NOTE: This test replicates #314 approx 1 in every 2 attempts. */
  "MVar" should {
    "have deterministic sequential take/put behaviour" in {
      def run = for {
        in <- newEmptyMVar[String]
        _ <- forkIO {
          for {
            _ <- in.put("one")
            _ <- in.put("two")
          } yield ()
        }
        a <- in.take
        b <- in.take
      } yield (a, b)

      run.unsafePerformIO must_== ("one", "two")
    }
  }
}
