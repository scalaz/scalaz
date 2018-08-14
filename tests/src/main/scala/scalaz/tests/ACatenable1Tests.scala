package scalaz
package tests

import scala.Int
import scala.Predef.{ Map, String}

import data._
import tc._
import Scalaz._

import laws._

import testz._
import z._

import ACatenable1.lift

object ACatenable1Tests {
  // these type aliases unnerve me with their presence
  type R[A, B] = ACatenable1[Map, A, B] => Map[A, B]
  type L[A, B] = Map[A, B] => ACatenable1[Map, A, B]
  val foldF: ACatenable1[Map, ?, ?] ~~> Map = Forall2.of[R](_.fold)
  val liftF: Map ~~> ACatenable1[Map, ?, ?] = Forall2.of[L](ACatenable1.lift(_))

  // specified in `compose`-order
  val fst = Map(
    (6, 9),
    (7, 10),
    (8, 11)
  )
  val snd = Map(
    (3, 6),
    (4, 7),
    (5, 8),
  )
  val thd = Map(
    (0, 3),
    (1, 4),
    (2, 5),
  )

  def const(s: String): ACatenable1[Biconst[String, ?, ?], Int, Int] = lift(Biconst(s))

  def tests[T](harness: Harness[T], sequence: (T, T) => T): T = {
    import harness._

    sequence(
      section("laws")(
        section("lawful semicategory")(
          test("associativity") { () =>
            SemicategoryLaws.composeAssoc(fst, snd, thd)(assertEqualNonEmptyMaps)
          },
        ),
        section("free semicategory")(
          test("foldMap") { () =>
            HomomorphismLaws.semicategoryCompose(foldF)(
              lift(fst) <<< lift(snd), lift(thd)
            )(assertEqualNonEmptyMaps) |+|
            HomomorphismLaws.semicategoryCompose(foldF)(
              lift(fst), lift(snd) <<< lift(thd)
            )(assertEqualNonEmptyMaps)
          },
          test("lift") { () =>
            HomomorphismLaws.semicategoryCompose(liftF)(
              fst, snd
            )((f, s) => assertEqualNonEmptyMaps(f.fold, s.fold))
          },
        ),
      ),
      section("tests")(
        // tests that order is correct.
        test("compose") { () =>
          assertEqual(
            const("hello").compose(const("world")).fold,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        // also tests that order is correct.
        test("andThen") { () =>
          assertEqual(
            const("world").andThen(const("hello")).fold,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        // tests that order is correct.
        test(":+") { () =>
          assertEqual(
            (const("hello") :+ Biconst[String, Int, Int]("world")).fold,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        // also tests that order is correct.
        test("+:") { () =>
          assertEqual(
            (Biconst[String, Int, Int]("hello") +: const("world")).fold,
            Biconst[String, Int, Int]("helloworld")
          )
        },

        test("foldLeft") { () =>
         assertEqual(
            (const("hello") >>> const("world") >>> const("foobar")).foldLeft(Const[String, Int](""))(
              ν[RightAction[Const[String, ?], Biconst[String, ?, ?]]][α, β] {
                (ga, fab) => Const("(" + Const.run(ga) + "|" + Biconst.run(fab) + ")")
              }
            ),
            Const[String, Int]("(((|hello)|world)|foobar)")
          )
        },

        test("foldRight") { () =>
          assertEqual(
            (const("hello") >>> const("world") >>> const("foobar")).foldRight(Const[String, Int](""))(
              ν[LeftAction[Const[String, ?], Biconst[String, ?, ?]]][α, β] {
                (fab, gb) => Const[String, α]("(" + Biconst.run(fab) + "|" + Const.run(gb) + ")")
              }
            ),
            Const[String, Int]("(hello|(world|(foobar|)))")
          )
        },
        test("fold") { () =>
          final class Fake[A, B](val str: String)
          // not lawful, used to observe internal behavior of `fold`.
          implicit val fakeSemicategory: Semicategory[Fake] = instanceOf(new SemicategoryClass[Fake] {
            def compose[A, B, C](fst: Fake[B, C], snd: Fake[A, B]): Fake[A, C] =
              new Fake("(" + fst.str + "|" + snd.str + ")")
          })
          assertEqual(
            (lift(new Fake[Int, Int]("hello")) :+ new Fake[Int, Int]("world") :+ new Fake[Int, Int]("foo") :+ new Fake[Int, Int]("bar")).fold.str,
            "((hello|world)|(foo|bar))"
          ) |+| assertEqual(
            (new Fake[Int, Int]("hello") +: new Fake[Int, Int]("world") +: new Fake[Int, Int]("foo") +: lift(new Fake[Int, Int]("bar"))).fold.str,
            "((hello|world)|(foo|bar))"
          )
        }
      )
    )
  }
}
