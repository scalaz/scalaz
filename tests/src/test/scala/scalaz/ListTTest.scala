package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object ListTTest extends SpecLite {
  type ListTOpt[A] = ListT[Option, A]


  "::" ! forAll {
    (xs: List[Byte], x: Byte) =>
      (x :: ListT.fromList[Id.Id, Byte](xs)) must_=== ListT.fromList[Id.Id, Byte](x :: xs)
  }

  "scanLeft" ! forAll {
    (xs: List[Byte], x: Byte, f: (Byte, Byte) => Byte) =>
      ListT.fromList[Id.Id, Byte](xs).scanLeft(x)(f) must_=== ListT.fromList[Id.Id, Byte](xs.scanLeft(x)(f))
  }

  "collect" ! forAll {
    (xs: List[Byte], pf: PartialFunction[Byte, Byte]) =>
      ListT.fromList[Id.Id, Byte](xs).collect(pf) must_=== ListT.fromList[Id.Id, Byte](xs.collect(pf))
  }


  "fromList / toList" ! forAll {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).toList must_===(ass)
  }

  "fromList / toList" ! forAll {
    import Id._
    (as: List[Int]) =>
      ListT.fromList[Id, Int](as).toList must_===(as)
  }

  "filter all" ! forAll {
    (ass: ListT[List, Int]) =>
      ass.filter(_ => true) must_===(ass)
  }

  "isEmpty" ! forAll {
    (s: List[Int]) =>
      ListT.fromList(List(s)).isEmpty.forall(_ == s.isEmpty)
  }

  "filter none" ! forAll {
    (ass: ListT[List, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.forall(_ == true)
  }

  "drop" ! forAll {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).drop(x).toList must_===(ass.map(_.drop(x)))
  }

  "take" ! forAll {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).take(x).toList must_===(ass.map(_.take(x)))
  }

  "mapM" ! forAll {
    (s: List[Int], l: List[Int]) =>
      val s0 = s map (_ + 1)
      ListT.fromList(List(s, s0)).mapM(i => l.map(_ + i)).toList must_==(
        Traverse[List].traverse(s)(i => l.map(_ + i)) :::
        Traverse[List].traverse(s0)(i => l.map(_ + i))
      )
  }

  "foldMap" ! forAll {
    (s: List[Int]) =>
      import scalaz.Scalaz._
      ListT.fromList(s.some).foldMap(_.toString) must_==(s.foldMap(_.toString))
  }

  "trampolined ListT" should {
    import Free.Trampoline

    val n = 100000L
    val s = ListT.unfoldM[Trampoline, Long, Long](n)(i =>
      Trampoline.done(if(i > 0) Some((i, i-1)) else None))

    val expected = n*(n+1)/2

    "not stack overflow on foldLeft" in {
      s.foldLeft(0L)((x, y) => x + y).run must_=== expected
    }

  }

  checkAll(equal.laws[ListTOpt[Int]])
  checkAll(monoid.laws[ListTOpt[Int]])
  checkAll(monadPlus.laws[ListTOpt])
  checkAll(foldable.laws[ListTOpt])

  "ListT[Id, _] with 100,000 initial skips" should {
    import Id.Id

    val s = {
      def nastyStream(n: Int, value: String): ListT[Id, String] =
        if(n > 0) nastyStream(n-1, value)
        else value :: ListT.empty[Id, String]

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
      s.tailMRec.isEmpty must_=== true
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

    "not stack-overflow on toListRec" in {
      s.toListRec must_=== List("foo")
    }

  }

  object instances {
    def semigroup[F[_]: Functor, A] = Semigroup[ListT[F, A]]
    def monoid[F[_]: Applicative, A] = Monoid[ListT[F, A]]
    def functor[F[_]: Functor] = Functor[ListT[F, *]]
    def bind[F[_]: Functor] = Bind[ListT[F, *]]
    def plus[F[_]: Functor] = Plus[ListT[F, *]]
    def monad[F[_]: Applicative] = Monad[ListT[F, *]]
    def monadPlus[F[_]: Applicative] = MonadPlus[ListT[F, *]]
    def foldable[F[_]: Foldable] = Foldable[ListT[F, *]]

    // checking absence of ambiguity
    def semigroup[F[_]: Applicative, A] = Semigroup[ListT[F, A]]
    def functor[F[_]: Applicative] = Functor[ListT[F, *]]
    def bind[F[_]: Applicative] = Bind[ListT[F, *]]
    def plus[F[_]: Applicative] = Plus[ListT[F, *]]
  }
}
