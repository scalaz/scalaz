package scalaz
package tests

import scala.{ List, Range, StringContext }
import scala.Predef.String

import Scalaz._

import testz._, z._
import laws._, FunctorLaws._

import zio._

object IOTests {

  case class Error(msg: String)

  // a probably-too-large selection of IO actions; more could be added
  val ios = {
    type IOES = IO[Error, String]
    type Endo = IOES => IOES
    val seeds = List[IOES](
      IO.now("now"),
      IO.unit.map(_ => "unit"),
      IO.point("point"),
      IO.fail(Error("error")),
      IO.sync("sync"),
      IO.async[Error, String](_(ExitResult.Completed("async success"))),
      IO.async[Error, String](_(ExitResult.Failed(Error("async failüre")))),
      //IO.sleep(scala.concurrent.duration.Duration.fromNanos(1)).map(_ => "slept")
    )
    val endos0: List[Endo] =
      seeds.map(s => (io: IOES) => io.flatMap(_ => s)) :::
        List(fns.s1, fns.s2, fns.f).map(f => (io: IOES) => io.flatMap(f)) :::
        List[Endo](
        _.map(_ + "ishly"),
      )
    val endos = Range(1, endos0.size)
      .flatMap(endos0.combinations)
      .flatMap(l => l)
    seeds.flatMap(s => endos.map(_(s)))
  }

  object RTS extends RTS

  def ioEql[E, A](l: IO[E, A], r: IO[E, A]): Result =
    assert(RTS.unsafeRunSync(l.attempt) == RTS.unsafeRunSync(r.attempt))

  object fns {
    val s1 = (s: String) => IO.point(s"foo$s")
    val s2 = (s: String) => IO.point(s + s)
    val f  = (s: String) => IO.fail(Error(s))
  }

  def tests[T](harness: Harness[T]): T = {
    import harness._
    namedSection("zio.IO instances")(
      namedSection("bifunctor")(
        test("identityToIdentity") { () =>
          ios.foldMap(Bifunctor.identityToIdentity(_)(ioEql))
        },
      ),
      namedSection("functor")(
        test("identity") { () =>
          ios.foldMap(Functor.identityToIdentity(_)(ioEql))
        },
      ),
      namedSection("applicative")(
        test("applyIdentity") { () =>
          ios.foldMap(ApplicativeLaws.applyIdentity(_)(ioEql))
        },
      ),
      namedSection("bind")(
        namedSection("bindAssoc")(
          test("success/success") { () =>
            ios.foldMap(BindLaws.bindAssoc(_)(fns.s1, fns.s2)(ioEql))
          },
          test("failure/success") { () =>
            ios.foldMap(BindLaws.bindAssoc(_)(fns.f, fns.s2)(ioEql))
          },
          test("failure/failure") { () =>
            ios.foldMap(BindLaws.bindAssoc(_)(fns.f, fns.f)(ioEql))
          },
          test("success/failure") { () =>
            ios.foldMap(BindLaws.bindAssoc(_)(fns.s1, fns.f)(ioEql))
          },
        )
      ),
      namedSection("monad")(
        namedSection("bindLeftIdentity")(
          test("success") { () =>
            MonadLaws.bindLeftIdentity("mxyzptlk")(fns.s1)(ioEql)
          },
          test("failure") { () =>
            MonadLaws.bindLeftIdentity("mxyzptlk")(fns.f)(ioEql)
          },
        ),
        namedSection("bindRightIdentity")(
          test("success") { () =>
            ios.foldMap(MonadLaws.bindRightIdentity(_)(ioEql))
          },
          test("failure") { () =>
            ios.foldMap(MonadLaws.bindRightIdentity(_)(ioEql))
          },
        ),
      )
    )
  }

}
