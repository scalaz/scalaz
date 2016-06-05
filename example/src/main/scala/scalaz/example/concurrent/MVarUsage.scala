package scalaz
package example
package concurrent

import scalaz._
import effect._
import IO._
import concurrent._
import MVar._
import std.anyVal._
import syntax.equal._

object MVarUsage extends App {
  def forkIO(f: => IO[Unit])(implicit s: Strategy): IO[Unit] = IO { s(f.unsafePerformIO); () }

  def out() {
    def calc(mvar: MVar[Int]): IO[Unit] = mvar.put(42)

    val io =
      for {
        mvar <- newEmptyMVar[Int]
        _ <- forkIO(calc(mvar))
        a <- mvar.take
      } yield a
    assert(io.unsafePerformIO === 42)
  }

  def inout() {
    def calc(in: MVar[Int], out: MVar[Int]): IO[Unit] =
      for {
        a <- in.take
        b <- in.take
        _ <- out.put(a * b)
      } yield ()

    val io =
      for {
        in  <- newMVar(6)
        out <- newEmptyMVar[Int]
        _   <- forkIO(calc(in, out))
        _   <- in.put(7)
        a   <- out.take
      } yield a
    assert(io.unsafePerformIO === 42)
  }

  def pingpong() {
    def pong(c: MVar[String], p: MVar[String]) =
      for {
        _ <- c.take flatMap (s => putStrLn("c: " + s))
        _ <- p.put("pong")
        _ <- c.take flatMap (s => putStrLn("c: " + s))
        _ <- p.put("pong")
      } yield ()

    def io =
      for {
        c <- newMVar("ping")
        p <- newEmptyMVar[String]
        _ <- forkIO(pong(c, p))
        _ <- p.take flatMap (s => putStrLn("p: " + s))
        _ <- c.put("ping")
        _ <- p.take flatMap (s => putStrLn("p: " + s))
      } yield ()
    io.unsafePerformIO
  }

  //out()
  //inout()
  pingpong()
}
