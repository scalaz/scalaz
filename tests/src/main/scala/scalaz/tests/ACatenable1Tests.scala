package scalaz
package tests

import scala.Int
import scala.Predef.{ Map, String }

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
  val foldBalancedF: ACatenable1[Map, ?, ?] ~~> Map = Forall2.of[R](_.foldBalanced)
  val liftF: Map ~~> ACatenable1[Map, ?, ?]         = Forall2.of[L](lift(_))

  // specified in `compose`-order
  // while these are each similar to `(+) 3`,
  // they are not commutative.
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

  def tests[T](harness: Harness[T]): T = {
    import harness._

    section(
      namedSection("instances")(
        namedSection("lawful semicategory")(
          test("associativity") { () =>
            SemicategoryLaws.composeAssoc(lift(fst), lift(snd), lift(thd))(
              (a, b) => assertEqualNonEmptyMaps(a.foldBalanced, b.foldBalanced)
            )
          },
        ),
        namedSection("free semicategory")(
          test("foldMap") { () =>
            HomomorphismLaws.semicategoryCompose(foldBalancedF)(
              lift(fst) <<< lift(snd),
              lift(thd)
            )(assertEqualNonEmptyMaps) |+|
              HomomorphismLaws.semicategoryCompose(foldBalancedF)(
                lift(fst),
                lift(snd) <<< lift(thd)
              )(assertEqualNonEmptyMaps)
          },
          test("lift") { () =>
            HomomorphismLaws.semicategoryCompose(liftF)(
              fst,
              snd
            )((f, s) => assertEqualNonEmptyMaps(f.foldBalanced, s.foldBalanced))
          },
        ),
      ),
      namedSection("tests")(
        // tests that order is correct.
        test("compose") { () =>
          assertEqual(
            const("world").compose(const("hello")).foldBalanced,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        // also tests that order is correct.
        test("andThen") { () =>
          assertEqual(
            const("hello").andThen(const("world")).foldBalanced,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        // tests that order is correct.
        test(":+") { () =>
          assertEqual(
            (const("hello") :+ Biconst[String, Int, Int]("world")).foldBalanced,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        // also tests that order is correct.
        test("+:") { () =>
          assertEqual(
            (Biconst[String, Int, Int]("hello") +: const("world")).foldBalanced,
            Biconst[String, Int, Int]("helloworld")
          )
        },
        test("foldLeft") { () =>
          assertEqual(
            (const("hello") >>> const("world") >>> const("foo")).foldLeft(Const[String, Int]("bar"))(
              ν[RightAction[Const[String, ?], Biconst[String, ?, ?]]][α, β] { (ga, fab) =>
                Const("(" + Const.run(ga) + "|" + Biconst.run(fab) + ")")
              }
            ),
            Const[String, Int]("(((bar|hello)|world)|foo)")
          )
        },
        test("foldRight") { () =>
          assertEqual(
            (const("hello") >>> const("world") >>> const("foo")).foldRight(Const[String, Int]("bar"))(
              ν[LeftAction[Const[String, ?], Biconst[String, ?, ?]]][α, β] { (fab, gb) =>
                Const[String, α]("(" + Biconst.run(fab) + "|" + Const.run(gb) + ")")
              }
            ),
            Const[String, Int]("(hello|(world|(foo|bar)))")
          )
        },
        test("foldBalanced") { () =>
          final class Fake[A, B](val str: String)
          // not lawful, used to observe balanced binary tree shape of `foldBalanced`.
          // not associative.
          implicit val fakeSemicategory: Semicategory[Fake] = instanceOf(new SemicategoryClass[Fake] {
            def compose[A, B, C](fst: Fake[B, C], snd: Fake[A, B]): Fake[A, C] =
              new Fake("(" + snd.str + "|" + fst.str + ")")
          })
          def fake(str: String): Fake[Int, Int] =
            new Fake(str)
          val result = "((hello|world)|(foo|bar))"
          IList(
            (lift(fake("hello")) :+ fake("world") :+ fake("foo") :+ fake("bar")).foldBalanced.str,
            (fake("hello") +: fake("world") +: fake("foo") +: lift(fake("bar"))).foldBalanced.str,
          ).foldMapStrict(assertEqual(_, result))
        }
      )
    )
  }
}
