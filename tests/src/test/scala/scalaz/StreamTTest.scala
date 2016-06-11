package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object StreamTTest extends SpecLite {
  type StreamTOpt[A] = StreamT[Option, A]

  "fromStream / toStream" ! forAll {
    (ass: Stream[Stream[Int]]) =>
      StreamT.fromStream(ass).toStream must_===(ass)
  }

  "fromStream / asStream" ! forAll {
    import Id._
    (as: Stream[Int]) =>
      StreamT.fromStream[Id, Int](as).asStream must_===(as)
  }

  "asStream" should {
    "be lazy" in {
      var highestTouched = 0

      val s1 = StreamT.unfold(1)(i => {
        highestTouched = math.max(i, highestTouched)
        if(i < 100) Some((i, i+1)) else None
      })

      val s2 = s1.asStream

      // test that at most 2 elements were evaluated in the conversion
      // (the fact that 2 are actually evaluated is a consequence of
      // how Stream.cons and StreamT.unfold are implemented)
      highestTouched mustBe_< 3
    }
  }

  "filter all" ! forAll {
    (ass: StreamT[Stream, Int]) =>
      ass.filter(_ => true) must_===(ass)
  }

  "isEmpty" ! forAll {
    (s: Stream[Int]) =>
      StreamT.fromStream(List(s)).isEmpty.forall(_ == s.isEmpty)
  }

  "filter none" ! forAll {
    (ass: StreamT[Stream, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.forall(_ == true)
  }

  "drop" ! forAll {
    (ass: Option[Stream[Int]], x: Int) =>
      StreamT.fromStream(ass).drop(x).toStream must_===(ass.map(_.drop(x)))
  }

  "take" ! forAll {
    (ass: Option[Stream[Int]], x: Int) =>
      StreamT.fromStream(ass).take(x).toStream must_===(ass.map(_.take(x)))
  }

  "mapM" ! forAll {
    (s: Stream[Int], l: List[Int]) =>
      val s0 = s map (_ + 1)
      StreamT.fromStream(List(s, s0)).mapM(i => l.map(_ + i)).toStream must_==(
        Traverse[Stream].traverse(s)(i => l.map(_ + i)) :::
        Traverse[Stream].traverse(s0)(i => l.map(_ + i))
      )
  }

  "foldMap" ! forAll {
    (s: Stream[Int]) =>
      import scalaz.Scalaz._
      StreamT.fromStream(s.some).foldMap(_.toString) must_==(s.foldMap(_.toString))
  }

  "foldRightM" should {
    "be able to terminate early" in {
      val s = StreamT.fromIterable((1 to 100))
      var highestRead: Int = 0
      val res = s.foldRightM(false)((a, acc) => {
        highestRead = math.max(a, highestRead)
        a >= 5 || acc
      })
      res must_=== true
      highestRead must_=== 5
    }
  }

  "trampolined StreamT" should {
    import Free.Trampoline

    val n = 100000L
    val s = StreamT.unfoldM[Trampoline, Long, Long](n)(i =>
      Trampoline.done(if(i > 0) Some((i, i-1)) else None))

    val expected = n*(n+1)/2

    "not stack overflow on foldRight" in {
      s.foldRight(0L)((x, y) => x + y).run must_=== expected
    }

    "not stack overflow on foldRightM" in {
      s.foldRightM(Trampoline.done(0L))((x, y) => y.map(x + _)).run must_=== expected
    }

    "not stack overflow and terminate early on foldRightM" in {
      var lowestRead = n + 1
      val res = s.foldRightM(Trampoline.done(false))((a, acc) => {
        lowestRead = math.min(a, lowestRead)
        if(a <= n/2) Trampoline.done(true) else acc
      })
      res.run must_=== true
      lowestRead must_=== n/2
    }
  }

  checkAll(equal.laws[StreamTOpt[Int]])
  checkAll(monoid.laws[StreamTOpt[Int]])
  checkAll(monadPlus.laws[StreamTOpt])
  checkAll(foldable.laws[StreamTOpt])

  "StreamT[Id, _] with 100,000 initial skips" should {
    import Id.Id

    val s = {
      def nastyStream(n: Int, value: String): StreamT[Id, String] =
        if(n > 0) StreamT[Id, String](StreamT.Skip(nastyStream(n-1, value)))
        else value :: StreamT.empty[Id, String]

      nastyStream(100000, "foo")
    }

    "not stack-overflow on unconsRec" in {
      s.unconsRec.get._1 must_=== "foo"
    }

    "not stack-overflow on isEmptyRec" in {
      s.isEmptyRec must_=== false
    }

    "not stack-overflow on headRec" in {
      s.headRec must_=== "foo"
    }

    "not stack-overflow on headOptionRec" in {
      s.headOptionRec must_=== Some("foo")
    }

    "not stack-overflow on tailMRec" in {
      s.tailMRec.step match {
        case StreamT.Done() =>
        case other => sys.error("unexpected " + other)
      }
    }

    "not stack-overflow on foreachRec" in {
      var acc = ""
      s.foreachRec(a => acc += a)
      acc must_=== "foo"
    }

    "not stack-overflow on foldRightRec" in {
      s.foldRightRec("")((a, b) => a + b) must_=== "foo"
    }

    "not stack-overflow on foldLeftRec" in {
      s.foldLeftRec("")((b, a) => b + a) must_=== "foo"
    }

    "not stack-overflow on lengthRec" in {
      s.lengthRec must_=== 1
    }

    "not stack-overflow on toStreamRec" in {
      s.toStreamRec.toList must_=== List("foo")
    }

    "not stack-overflow on asStream" in {
      s.asStream.toList must_=== List("foo")
    }
  }

  object instances {
    def semigroup[F[_]: Functor, A] = Semigroup[StreamT[F, A]]
    def monoid[F[_]: Applicative, A] = Monoid[StreamT[F, A]]
    def functor[F[_]: Functor] = Functor[StreamT[F, ?]]
    def bind[F[_]: Functor] = Bind[StreamT[F, ?]]
    def plus[F[_]: Functor] = Plus[StreamT[F, ?]]
    def monad[F[_]: Applicative] = Monad[StreamT[F, ?]]
    def monadPlus[F[_]: Applicative] = MonadPlus[StreamT[F, ?]]
    def foldable[F[_]: Foldable] = Foldable[StreamT[F, ?]]

    // checking absence of ambiguity
    def semigroup[F[_]: Applicative, A] = Semigroup[StreamT[F, A]]
    def functor[F[_]: Applicative] = Functor[StreamT[F, ?]]
    def bind[F[_]: Applicative] = Bind[StreamT[F, ?]]
    def plus[F[_]: Applicative] = Plus[StreamT[F, ?]]
  }
}
