package scalaz
package example
package concurrent

import scalaz._
import effect._
import concurrent._
import Chan._
import std.anyVal._
import syntax.equal._

object ChanUsage extends App {
  def forkIO(f: => IO[Unit])(implicit s: Strategy): IO[Unit] = IO { s(f.unsafePerformIO); () }

  def calc(chan: Chan[Int], a: Int) =
    chan.write((1 to a).sum)

  val io =
    for {
      chan <- newChan[Int]
      _ <- forkIO(calc(chan, 100))
      _ <- forkIO(calc(chan, 200))
      a <- chan.read
      b <- chan.read
    } yield a + b
  assert(io.unsafePerformIO === 25150)
}
