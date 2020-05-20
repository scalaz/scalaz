package scalaz

import scalaz.State._
import std.AllInstances._
import syntax.traverse._

object TraverseTestJVM extends SpecLite {

  "list" should {
    "sequenceS, traverseS, traversalS does not blow stack" in {
      val N = 100000
      val F = new Traverse[List]{
        def traverseImpl[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]) =
          Traverse[List].traverseImpl(fa)(f)
      }
      val s = List.fill(N)(modify((_: Int) + 1))
      F.sequenceS(s).exec(0) must_=== N
      F.traverseS(s)(x => x).exec(0) must_=== N
      F.traversalS[Int].run(s)(x => x).exec(0) must_=== N
    }
  }

  "combos" should {
    "traverse large LazyList over trampolined StateT including IO" in {
      // Example usage from Eric Torreborre
      import scalaz.effect._

      val as = LazyList.range(0, 100000)
      val state: State[Int, IO[LazyList[Int]]] = as.traverseSTrampoline[IO, Int, Int](a => for {
        s <- State.get[Int]
        _ <- State.put(a)
      } yield IO(a - s))
      state.eval(0).unsafePerformIO().take(3) must_===(LazyList(0, 1, 1))
    }
  }
}
