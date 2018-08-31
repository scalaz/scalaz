package scalaz
package tests

import Predef.Int
import data._

import Scalaz._
import AMaybe.{empty, just}

import testz._

object AMaybeTests {
  trait Point[F[_]] {
    def point[A](a: A): F[A]
  }

  trait FreePoint[A, B] {
    def doIt[F[_]](point: Point[F])(a: A): F[A]
  }

  val listPoint = new Point[IList] {
    def point[A](a: A): IList[A] = IList(a)
  }

  def tests[T](harness: Harness[T], sequence: (T, T) => T): T = {
    import harness._
    sequence(
      section("debug instance")(
        test("AEmpty") { () =>
          assert(empty[(?, ?), Int].debugs === "AEmpty")
        },
        test("AJust") { () =>
          assert(just[(?, ?), Int, Int]((0, 1)).debugs === "AJust((0, 1))")
        },
      ),
      test("empty is cached") { () =>
        assert(empty[(?, ?), Int] eq empty[(?, ?), Int])
      }
    )
  }
}
