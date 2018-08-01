package scalaz
import org.scalacheck.Prop.forAll


object TraverseTest extends SpecLite {

  import scalaz._
  import scalaz.State._
  import std.AllInstances._
  import std.AllFunctions._
  import syntax.traverse._

  "list" should {
    // ghci> import Data.Traversable
    // ghci> import Control.Monad.Writer
    // ghci> let (|>) = flip ($)
    // ghci> traverse (\x -> writer (x, x)) ["1", "2", "3"] |> runWriter
    // (["1","2","3"],"123")
    "apply effects in order" in {
      val s: Writer[String, List[Int]] = List(1, 2, 3).traverseU(x => Writer(x.toString, x))
      s.run must_===(("123", List(1, 2, 3)))
    }

    "indexed" ! forAll { xs: List[Byte] =>
      Traverse[List].indexed(xs) must_=== xs.zipWithIndex.map{case (a, b) => (b, a)}
    }

    "traverse through option effect" in {
      val s: Option[List[Int]] = List(1, 2, 3).traverseU((x: Int) => if (x < 3) some(x) else none)
      s must_===(none[List[Int]])
    }

    "traverse int function as monoidal applicative" in {
      val s: Const[Int, _] = List(1, 2, 3) traverseU {a => Const(a + 1)}
      s.getConst must_===(9)
    }

    "not blow the stack" in {
      val s: Option[List[Int]] = List.range(0, 32 * 1024).traverseU(x => some(x))
      s.map(_.take(3)) must_===(some(List(0, 1, 2)))
    }

    "be stack-safe and short-circuiting" in {
      val N = 10000
      val s: Maybe[List[Int]] = List.range(0, N) traverse { x =>
        if(x < N-2) Maybe.just(x)
        else if(x == N-2) Maybe.empty
        else sys.error("BOOM!")
      }
      s must_=== Maybe.empty
    }

    "state traverse agrees with regular traverse" in {
      val N = 10
      List.range(0,N).traverseS(x => modify((x: Int) => x+1))(0) must_=== (
      List.range(0,N).traverseU(x => modify((x: Int) => x+1)).apply(0))
    }

    "state traverse does not blow stack" in {
      val N = 10000
      val s = List.range(0,N).traverseS(x => modify((x: Int) => x+1))
      s.exec(0) must_=== (N)
    }

  }

  "ilist" should {
    "be stack-safe and short-circuiting" in {
      val N = 10000
      val s: Maybe[IList[Int]] = IList.fromList(List.range(0, N)) traverse { x =>
        if(x < N-2) Maybe.just(x)
        else if(x == N-2) Maybe.empty
        else sys.error("BOOM!")
      }
      s must_=== Maybe.empty
    }
  }

  "stream" should {
    "apply effects in order" in {
      val s: Writer[String, Stream[Int]] = Stream(1, 2, 3).traverseU(x => Writer(x.toString, x))
      s.run must_===(("123", Stream(1, 2, 3)))
    }

    "be stack-safe and short-circuiting" in {
      val N = 10000
      val s: Maybe[Stream[Int]] = Stream.from(0) traverse { x =>
        if(x < N-2) Maybe.just(x)
        else if(x == N-2) Maybe.empty
        else sys.error("BOOM!")
      }
      s must_=== Maybe.empty
    }
  }

  "ephemeralstream" should {
    "be stack-safe and short-circuiting" in {
      val N = 10000
      val s: Maybe[EphemeralStream[Int]] = EphemeralStream.fromStream(Stream.from(0)) traverse { x =>
        if(x < N-2) Maybe.just(x)
        else if(x == N-2) Maybe.empty
        else sys.error("BOOM!")
      }
      s must_=== Maybe.empty
    }
  }

  "nonemptylist" should {
    "be stack-safe and short-circuiting" in {
      val N = 10000
      val fa = NonEmptyList.nel(0, IList.fromList(List.range(1, N)))
      val s: Maybe[NonEmptyList[Int]] = Traverse1[NonEmptyList].traverse1 (fa) ({ x =>
        if(x < N-2) Maybe.just(x)
        else if(x == N-2) Maybe.empty
        else sys.error("BOOM!")
      })
      s must_=== Maybe.empty
    }
  }

  "combos" should {
    "traverse with monadic join" in {
      val s: Writer[String, List[Int]] = List(1, 2, 3).traverseM[Writer[String, ?], Int](x => Writer(x.toString, List(x, x * 2)))
      s.run must_===(("123", List(1, 2, 2, 4, 3, 6)))
    }
  }

  "derived functions" should {
    "sequence" in {
      some(List(1, 2, 3)).sequence must_===(List(some(1), some(2), some(3)))
      List(some(1), some(2)).sequence must_===(some(List(1, 2)))
      List(some(1), none[Int]).sequence must_===(none)

      val states: List[State[Int, Int]] = List(State.modify[Int](_ + 1).map(_ => 0), for {
          i <- State.get[Int]
          _ <- State.put(i + 1)
        } yield i)
      val state: State[Int, List[Int]] = states.sequenceU
      state.run(0) must_===(2 -> List(0, 1))

      List(some(List(1, 2)), some(List(3, 4, 5))).sequenceM must_===(some(List(1, 2, 3, 4, 5)))
      List(some(List(1, 2)), none[List[Int]]).sequenceM must_===(none)
    }

    "reverse" in {
      Traverse[List].reverse(List(1, 2, 3)) must_===(List(3, 2, 1))
    }

    "mapAccumL/R" ! forAll {
      val L = Traverse[List]; import L.traverseSyntax._
      (l: List[Int]) => {
        val (acc, l2) = l.mapAccumL(List[Int]())((acc,a) => (a :: acc, a))
        val (acc2, l3) = l.mapAccumR(List[Int]())((acc,a) => (a :: acc, a))
        acc == l.reverse && l2 == l && acc2 == l3 && l3 == l
      }
    }

    "double reverse" ! forAll {
      (is: List[Int]) =>
        import syntax.monoid._
        Endo(Traverse[List].reverse[Int]).multiply(2).apply(is) must_===(is)
    }
  }
}
