package scalaz
package example
package concurrent

import scalaz._
import effect._
import IO._
import concurrent._
import Chan._

object ChanUsage extends App {
  def forkIO(f: => IO[Unit])(implicit s: Strategy): IO[Unit] = IO { s(f unsafePerformIO); () }

  def calc(chan: Chan[Int], a: Int) = 
    for {
      _ <- putStrLn("summing 1 to " + a)
      val s = (1 to a).sum
      _ <- putStrLn("done summing 1 to " + a)
      _ <- chan.write(s)
      _ <- putStrLn("wrote sum")
    } yield ()

  val io =
    for {
      chan <- newChan[Int]
      _ <- forkIO(calc(chan, 100))
      _ <- forkIO(calc(chan, 200))
      _ <- putStrLn("reading first result")
      a <- chan.read
      _ <- putStrLn("reading second result")
      b <- chan.read
      _ <- putStrLn("total: " + (a + b))
    } yield ()
  io.unsafePerformIO
}
