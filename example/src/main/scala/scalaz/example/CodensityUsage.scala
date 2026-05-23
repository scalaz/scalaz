package scalaz.example

import scalaz._, Scalaz._
import effect._

// Example of using Codensity for resource management
object CodensityUsage extends SafeApp {
  def bracketCodensity[A, B](
    before: IO[A])(after: A => IO[B]
  ): Codensity[IO, A] =
    new Codensity[IO, A] {
      def apply[C](f: A => IO[C]): IO[C] =
        before.bracket(after)(f)
    }

  def resource(s: String): Codensity[IO, String] =
    bracketCodensity(
      IO.putStrLn(s"Acquire resource: $s") >| s
    )(
      s0 => IO.putStrLn(s"Release resource: $s0")
    )

  val prg = for {
    r1 <- resource("R1")
    r2 <- resource("R2")
    rs <- (3 to 6).toList.traverseU(s => resource(s"R$s"))
    _ <- IO.putStrLn("Acquired resources:").liftM[Codensity]
    _ <- (r1 :: r2 :: rs).traverse(IO.putStrLn).liftM[Codensity]
  } yield ()

  override def runc = prg.improve
}
