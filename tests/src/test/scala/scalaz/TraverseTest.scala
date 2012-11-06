package scalaz

import scalacheck.ScalazProperties

class TraverseTest extends Spec {

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
      s.run must be_===(("123", List(1, 2, 3)))
    }

    "traverse through option effect" in {
      val s: Option[List[Int]] = List(1, 2, 3).traverseU((x: Int) => if (x < 3) some(x) else none)
      s must be_===(none[List[Int]])
    }

    "traverse int function as monoidal applicative" in {
      val s: Int = List(1, 2, 3) traverseU {_ + 1}
      s must be_===(9)
    }

    "not blow the stack" in {
      val s: Option[List[Int]] = List.range(0, 32 * 1024).traverseU(x => some(x))
      s.map(_.take(3)) must be_===(some(List(0, 1, 2)))
    }

    "state traverse agrees with regular traverse" in {
      var N = 10
      List.range(0,N).traverseS(x => modify((x: Int) => x+1))(0) must be_=== (
      List.range(0,N).traverseU(x => modify((x: Int) => x+1)).apply(0))
    }

    "state traverse does not blow stack" in {
      var N = 10000
      val s = List.range(0,N).traverseS(x => modify((x: Int) => x+1))
      s.exec(0) must be_=== (N)
    }
  }

  "stream" should {
    "apply effects in order" in {
      val s: Writer[String, Stream[Int]] = Stream(1, 2, 3).traverseU(x => Writer(x.toString, x))
      s.run must be_===(("123", Stream(1, 2, 3)))
    }

    // ghci> import Data.Traversable
    // ghci> traverse (\x -> if x < 3 then Just x else Nothing) [1 ..]
    // Nothing
    "allow partial traversal" in {
      val stream = Stream.from(1)
      val s: Option[Stream[Int]] = stream.traverseU((x: Int) => if (x < 3) some(x) else none)
      s must be_===(none)
    }
  }

  "combos" should {
    "traverse large stream over trampolined StateT including IO" in {
      // Example usage from Eric Torreborre
      import scalaz.effect._

      val as = Stream.range(0, 100000)
      val state: State[Int, IO[Stream[Int]]] = as.traverseSTrampoline[IO, Int, Int](a => (for {
        s <- State.get[Int]
        _ <- State.put(a)
      } yield IO(a - s)): State[Int, IO[Int]])
      state.eval(0).unsafePerformIO().take(3) must be_===(Stream(0, 1, 1))
    }
  }

  "derived functions" should {
    "sequence" in {
      some(List(1, 2, 3)).sequence must be_===(List(some(1), some(2), some(3)))
      List(some(1), some(2)).sequence must be_===(some(List(1, 2)))
      List(some(1), none[Int]).sequence must be_===(none)

      val states: List[State[Int, Int]] = List(State.modify[Int](_ + 1).map(_ => 0), for {
          i <- State.get[Int]
          _ <- State.put(i + 1)
        } yield i)
      val state: State[Int, List[Int]] = states.sequenceU
      state.run(0) must be_===(2, (List(0, 1)))
    }

    "reverse" in {
      Traverse[List].reverse(List(1, 2, 3)) must be_===(List(3, 2, 1))
    }

    "mapAccumL/R" ! check {
      val L = Traverse[List]; import L.traverseSyntax._
      (l: List[Int]) => {
        val (acc, l2) = l.mapAccumL(List[Int]())((acc,a) => (a :: acc, a))
        val (acc2, l3) = l.mapAccumR(List[Int]())((acc,a) => (a :: acc, a))
        acc == l.reverse && l2 == l && acc2 == l3 && l3 == l
      }
    }

    "double reverse" ! check {
      (is: List[Int]) =>
        import syntax.monoid._
        Endo(Traverse[List].reverse[Int]).multiply(2).apply(is) must be_===(is)
    }
  }
}
