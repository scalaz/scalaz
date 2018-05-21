package scalaz.effect

import org.specs2.Specification
import scalaz.data.Disjunction.-\/

class PromiseSpec extends Specification with RTS {

  def is = "PromiseSpec".title ^ s2"""
        Make a promise and retrieve its value correctly after complete it with:
          `complete` to complete that promise with a specified value. $e1
          `done` to complete that promise with a completed result.    $e2

        Make a promise and retrieve its fail value after complete it with:
          `error` to fail that promise with a specified error.  $e3
          `done` to complete that promise with a failed result. $e4

        Given a completed promise `done` returns false and get should return the first completed value. $e5

        Make a promise and retrieve its Throwable value after interruption calling:
          `done` to complete that promise with a terminated result.              $e6
          `interrupt` with a specified throwable and interrupt all other fibers. $e7

     """

  def e1 =
    unsafePerformIO(
      for {
        p <- Promise.make[Void, Int]
        f <- p.complete[Void](32).fork
        s <- f.join
        v <- p.get
      } yield s must beTrue and (v must_=== 32)
    )

  def e2 =
    unsafePerformIO(
      for {
        p <- Promise.make[Void, Int]
        f <- p.done[Void](ExitResult.Completed(14)).fork
        s <- f.join
        v <- p.get
      } yield s must beTrue and (v must_=== 14)
    )

  def e3 =
    unsafePerformIO(
      for {
        p <- Promise.make[String, Int]
        f <- p.error[String]("error in e3").fork
        s <- f.join
        v <- p.get.attempt[String]
      } yield s must beTrue and (v must_=== -\/("error in e3"))
    )

  def e4 =
    unsafePerformIO(
      for {
        p <- Promise.make[String, Int]
        f <- p.done[String](ExitResult.Failed("error in e4")).fork
        s <- f.join
        v <- p.get.attempt[String]
      } yield s must beTrue and (v must_=== -\/("error in e4"))
    )

  def e5 =
    unsafePerformIO(
      for {
        p <- Promise.make[Void, Int]
        _ <- p.complete[Void](1)
        s <- p.done[Void](ExitResult.Completed(9))
        v <- p.get
      } yield s must beFalse and (v must_=== 1)
    )

  lazy val error = new Exception("Error!")
  def e6 =
    unsafePerformIO(
      for {
        p <- Promise.make[Exception, Int]
        s <- p.done[Exception](ExitResult.Terminated(error))
      } yield s must beTrue
    )
  def e7 =
    unsafePerformIO(
      for {
        p <- Promise.make[Exception, Int]
        s <- p.interrupt[Exception](error)
      } yield s must beTrue
    )
}
