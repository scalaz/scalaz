package scalaz
package tests

import Predef._
import data._
import laws._
import Scalaz._

import testz._
import z._

object MaybeTests {

  def tests[T](harness: Harness[T]): T = {
    import harness._

    val maybes =
      IList[Maybe[String]](
        Maybe.empty,
        Maybe.just("hello"),
        Maybe.just("world"),
        Maybe.just("foobar"),
      )

    section(
      namedSection("concrete")(
        namedSection("empty")(
          test("is cached")(
            () => assert(Maybe.empty.asInstanceOf[scala.AnyRef] eq Maybe.empty.asInstanceOf[scala.AnyRef])
          ),
          test("is `None`")(() => assert(Maybe.empty.asInstanceOf[scala.AnyRef] eq scala.None)),
        ),
        test("just is Some")(() => assert(Maybe.just(1) == scala.Some(1))),
        test("toOption is identity")(
          () =>
            maybes.foldMap { m =>
              assert(m.asInstanceOf[scala.AnyRef] eq Maybe.toOption(m))
          }
        ),
      ),
      namedSection("laws")(
        namedSection("eq laws")(
          test("reflexivity") { () =>
            maybes.foldMap(
              EqLaws.reflexivity(_)(assert)
            )
          },
          test("identity") { () =>
            maybes.cross(maybes).foldMap {
              case (m1, m2) => assert((m1 === m2) == (m1 == m2))
            }
          }
        ),
        namedSection("monad laws")(
          test("functor identity") { () =>
            maybes.foldMap(
              FunctorLaws.Functor.identityToIdentity(_)(assertEqual[Maybe[String]])
            )
          },
          test("apply associativity") { () =>
            val funs = maybes.map(_.map(i => (x: String) => x + i))
            maybes.cross(funs).cross(funs).foldMap {
              case ((m, f1), f2) =>
                ApplyLaws.applyAssoc(m)(f1, f2)(assertEqual[Maybe[String]])
            }
          },
          test("applicative identity") { () =>
            maybes.foldMap {
              ApplicativeLaws.applyIdentity(_)(assertEqual[Maybe[String]])
            }
          },
          test("bind associativity") { () =>
            val fun1 = (s: String) => Maybe.just(s + s)
            val fun2 = (s: String) => if (s.length != "hellohello".length) Maybe.empty[String] else Maybe.just(s)
            maybes.foldMap {
              BindLaws.bindAssoc(_)(fun1, fun2)(assertEqual[Maybe[String]])
            }
          },
          test("monad right identity") { () =>
            maybes.foldMap {
              MonadLaws.bindRightIdentity(_)(assertEqual[Maybe[String]])
            }
          },
          test("monad left identity") { () =>
            val fun = (i: Int) => if (i < 2) Maybe.empty[Int] else Maybe.just(i * 2)
            IList(1, 2, 3, 4, 5).foldMap {
              MonadLaws.bindLeftIdentity(_)(fun)(assertEqual[Maybe[Int]])
            }
          },
        ),
        namedSection("traversable laws")(
          test("traversable composition") { () =>
            val fun1: String => String \/ Int = s =>
              if (s == "hello") \/-(2)
              else if (s == "world") \/-(3)
              else -\/("unrecognized")
            val fun2 = (i: Int) => if (i < 4) IList(1, 2, 3) else IList(4, 5, 6)
            maybes.foldMap {
              TraversableLaws.traverseComposition[Maybe, String \/ ?, IList, String, Int, Int, Result](_)(fun1, fun2)(
                assertEqual(_, _)
              )
            }
          },
          test("traversable identity") { () =>
            maybes.foldMap {
              TraversableLaws.traverseIdentity(_)(assertEqual[Maybe[String]])
            }
          },
        ),
        namedSection("monoid laws")(
          test("mappend associativity") { () =>
            maybes.cross(maybes).cross(maybes).foldMap {
              case ((m1, m2), m3) =>
                SemigroupLaws.assoc(m1, m2, m3)(assertEqual[Maybe[String]])
            }
          },
          test("mappend left identity") { () =>
            maybes.foldMap {
              MonoidLaws.leftIdentity(_)(assertEqual[Maybe[String]])
            }
          },
          test("mappend right identity") { () =>
            maybes.foldMap {
              MonoidLaws.rightIdentity(_)(assertEqual[Maybe[String]])
            }
          },
        )
      )
    )
  }
}
