package scalaz
package tests

import scala.{ Exception, Int, List, Nothing, Range, StringContext, Throwable, Unit }
import scala.Predef.String
import Scalaz._
import testz._
import z._
import laws._
import FunctorLaws._
import scalaz.data._
import scalaz.data.io._
import zio._
import scala.concurrent.duration._
import Result._

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

  val exampleError    = new Exception("Oh noes!")
  val interruptCause1 = new Exception("Oh noes 1!")

  object RTS extends RTS {
    override def defaultHandler: List[Throwable] => IO[Nothing, Unit] = i => IO.traverse(i)(_ => IO.unit) *> IO.unit
  }

  object fns {
    val s1 = (s: String) => IO.point(s"foo$s")
    val s2 = (s: String) => IO.point(s + s)
    val f  = (s: String) => IO.fail(Error(s))
  }

  def tests[T](harness: Harness[T], rts: RTS): T = {
    import harness._

    def ioEql[E, A](l: IO[E, A], r: IO[E, A]): Result =
      assert(rts.unsafeRunSync(l.attempt) == rts.unsafeRunSync(r.attempt))

    namedSection("zio")(
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
      ),
      namedSection("zio.IO.type ops")(
        test("absolve") { () =>
          val res1 = IO.absolvez(IO.point(\/-(1)))
          val res2 = IO.absolvez(IO.point(-\/("fail")))

          combine(
            assertEqual(rts.unsafeRun(res1), 1),
            assertEqual[String \/ Int](rts.unsafeRun(res2.attemptz), -\/("fail"))
          )
        },
        test("fromDisjunction") { () =>
          val res1 = IO.fromDisjunction(\/-(1))
          val res2 = IO.fromDisjunction(-\/("fail"))

          combine(
            assertEqual(rts.unsafeRun(res1), 1),
            assertEqual[String \/ Int](rts.unsafeRun(res2.attemptz), -\/("fail"))
          )
        },
        test("fromMaybe") { () =>
          val res1 = IO.fromMaybe(Maybe.just(1))
          val res2 = IO.fromMaybe(Maybe.empty)

          combine(
            assertEqual(rts.unsafeRun(res1), 1),
            assertEqual[Unit \/ Int](rts.unsafeRun(res2.attemptz), -\/(()))
          )
        },
        test("require") { () =>
          val res1 = IO.requirez("fail")(IO.point(Maybe.just(1)))
          val res2 = IO.requirez("fail")(IO.point(Maybe.empty))

          combine(
            assertEqual(rts.unsafeRun(res1), 1),
            assertEqual[String \/ Int](rts.unsafeRun(res2.attemptz), -\/("fail"))
          )
        },
        test("forkAll") { () =>
          val list = IList(IO.point(1), IO.point(2))
          val res  = rts.unsafeRun(IO.forkAllz(list).flatMap(_.join))
          assertEqual(res, IList(1, 2))
        },
        test("raceAll") { () =>
          val list   = IList(IO.sleep(1.millisecond) *> IO.point(1))
          val slower = IO.sleep(1.second) *> IO.point(2)
          assertEqual(rts.unsafeRun(IO.raceAllz(slower, list)), 1)
        },
        test("reduceAll") { () =>
          val list: IList[IO[Nothing, Int]] = IList(IO.point(1), IO.point(2))
          assertEqual(rts.unsafeRun(IO.reduceAllz(IO.point(3), list)(_ + _)), 6)
        },
        test("mergeAll") { () =>
          val list: IList[IO[Nothing, Int]] = IList(IO.point(1), IO.point(2))
          assertEqual(rts.unsafeRun(IO.mergeAllz[Nothing, Int, Int, IList](list)(0, _ + _)), 3)
        },
        test("terminate0") { () =>
          val comp = IO.terminate0z[IList](IList(exampleError)).ensuring(IO.sync(throw interruptCause1))
          assert(rts.unsafeRun(comp.run) == ExitResult.Terminated(List(exampleError, interruptCause1)))
        },
        test("unsandbox") { () =>
          val defect = IO.unsandboxz[String, Int, IList](IO.fail(-\/(IList(exampleError)))).run
          val err    = IO.unsandboxz[String, Int, IList](IO.fail(\/-("fail"))).run
          val succ   = IO.unsandboxz[String, Int, IList](IO.point[Int](1)).run

          combine(
            assert(rts.unsafeRun(defect) == ExitResult.Terminated(List(exampleError))),
            combine(
              assert(rts.unsafeRun(err) == ExitResult.Failed("fail")),
              assert(rts.unsafeRun(succ) == ExitResult.Completed(1))
            )
          )
        }
      ),
      namedSection("zio.IO ops")(
        test("attempt") { () =>
          val succ: String \/ Int = rts.unsafeRun(IO.point[Int](1).attemptz)
          val fail: String \/ Int = rts.unsafeRun(IO.fail[String]("fail").attemptz)
          combine(
            assertEqual(succ, \/-[String, Int](1)),
            assertEqual(fail, -\/[String, Int]("fail"))
          )
        },
        test("raceBoth") { () =>
          val faster: IO[Int, String] = IO.sleep(1.millisecond) *> IO.point("1")
          val slower: IO[Int, String] = IO.sleep(1.second) *> IO.point("2")
          assertEqual(rts.unsafeRun(slower.raceBothz[Int, String](faster)), \/-[String, String]("1"))
        },
        test("or") { () =>
          val err  = IO.fail("fail")
          val succ = IO.point[Int](1)

          val error: String \/ (Int \/ Int) = rts.unsafeRun(err.or(err).attemptz)
          val succ1: String \/ Int          = rts.unsafeRun(err.or(succ))
          val succ2: Int \/ String          = rts.unsafeRun(succ.or(err))

          combine(
            assertEqual(error, -\/[String, Int \/ Int]("fail")),
            combine(
              assertEqual(succ1, \/-[String, Int](1)),
              assertEqual(succ2, -\/[Int, String](1))
            )
          )
        },
        test("sandboxed") { () =>
          val defect: IO[IList[Throwable] \/ String, Int] = IO.sync(throw exampleError).sandboxedz[IList]
          val err: IO[IList[Throwable] \/ String, Int]    = IO.fail("fail").sandboxedz[IList]
          val succ: IO[IList[Throwable] \/ String, Int]   = IO.point[Int](1).sandboxedz[IList]

          combine(
            assert(rts.unsafeRun(defect.attemptz) == -\/[IList[Throwable] \/ String, Int](-\/(IList(exampleError)))),
            combine(
              assert(rts.unsafeRun(err.attemptz) == -\/[IList[Throwable] \/ String, Int](\/-("fail"))),
              assertEqual(rts.unsafeRun(succ), 1)
            )
          )
        },
        test("sandboxWith") { () =>
          val defect =
            IO.sync(throw exampleError)
              .sandboxWithz[Nothing, Int, IList](
                (_: IO[IList[Throwable] \/ String, Int]) =>
                  IO.fail[IList[Throwable] \/ Nothing](-\/(IList(interruptCause1)))
              )
          val err =
            IO.fail("fail").sandboxWithz[Int, Unit, IList]((_: IO[IList[Throwable] \/ String, Int]) => IO.fail(\/-(-1)))
          val succ =
            IO.point[Int](1).sandboxWithz[Nothing, Int, IList]((_: IO[IList[Throwable] \/ String, Int]) => IO.point(1))

          combine(
            assert(rts.unsafeRun(defect.run) == ExitResult.Terminated(List(interruptCause1))),
            combine(
              assert(rts.unsafeRun(err.run) == ExitResult.Failed(-1)),
              assert(rts.unsafeRun(succ.run) == ExitResult.Completed(1))
            )
          )
        }
      )
    )

  }

}
