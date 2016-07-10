package scalaz
package effect

import java.io.{ StringReader, IOException }

object MonadCatchIOTest extends SpecLite {
  import Kleisli.kleisli
  import syntax.monad._
  import syntax.effect.monadCatchIO._
  import std.effect.closeable._

  // Some canned failures
  val err1 = new Exception("err1")
  val err2 = new Exception("err2")

  // A simple type that's not IO but does have a MonadCatchIO
  type T[A] = Kleisli[IO, Int, A]
  def ok[A](a: A): T[A] = kleisli(n => IO(a))
  def fail[A](e: Throwable): T[A] = kleisli(n => IO(throw e))

  // given two functions to take T[A] => T[B], the first using MonadCatchIO and the second
  // using syntax, return a function that consumes a T[A] and evaluates the corresponding T[B]
  // using the given predicate. Kind of contrived but it saves a lot of duplication and lets
  // us test the module and syntax at the same time.
  def mkTest[A,B](a: (MonadCatchIO.type, T[A]) => T[B], b: T[A] => T[B]): T[A] => (T[B] => Boolean) => Boolean =
    t => f => f(a(MonadCatchIO, t)) && f(b(t))

  // Used by the catchSome* tests below
  def catch1(t: Throwable): Option[String] =
    if (t == err1) Some(t.getMessage) else None

  "MonadCatchIO.catchSome" should {
    val test = mkTest[Int,Int](
      _.catchSome(_)(catch1, (s: String) => ok(s.length)),
      _.catchSome(catch1, (s: String) => ok(s.length)))
    "do nothing if nothing thrown" in {
      test(ok(3))(_.run(1).unsafePerformIO == 3)
    }
    "catch some exceptions" in {
      test(ok(3) >> fail[Int](err1))(_.run(1).unsafePerformIO == 4)
    }
    "not catch other exceptions" in {
      test(ok(3) >> fail[Int](err2)) { a =>
        try {
          a.run(1).unsafePerformIO
          fail("should have thrown")
        } catch {
          case t: Throwable => t eq err2
        }
      }
    }
  }

  "MonadCatchIO.catchLeft" should {
    val test = mkTest[Int, Throwable \/ Int](_.catchLeft(_), _.catchLeft)
    "do nothing if nothing thrown" in {
      test(ok(3))(_.run(1).unsafePerformIO == \/-(3))
    }
    "catch exceptions" in {
      test(ok(3) >> fail[Int](err1))(_.run(1).unsafePerformIO == -\/(err1))
    }
  }

  "MonadCatchIO.catchSomeLeft" should {
    val test = mkTest[Int, String \/ Int](_.catchSomeLeft(_)(catch1), _.catchSomeLeft(catch1))
    "do nothing if nothing thrown" in {
      test(ok(3))(_.run(1).unsafePerformIO == \/-(3))
    }
    "catch some exceptions" in {
      test(ok(3) >> fail[Int](err1))(_.run(1).unsafePerformIO == -\/(err1.getMessage))
    }
    "not catch other exceptions" in {
      test(ok(3) >> fail[Int](err2)) { a =>
        try {
          a.run(1).unsafePerformIO
          fail("should have thrown")
        } catch {
          case t: Throwable => t eq err2
        }
      }
    }
  }

  "MonadCatchIO.using" should {
    "close the resource properly" in {
      val r = new StringReader("abcdef")
      ok(r).using(_ => ok(42)).run(1).unsafePerformIO
      try {
        r.read
        fail("should have thrown")
      } catch {
        case ioe: IOException => // ok
      }
    }

  }

}

// vim: expandtab:ts=2:sw=2
