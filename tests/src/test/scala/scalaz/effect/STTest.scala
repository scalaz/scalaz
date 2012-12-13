package scalaz
package effect

import std.AllInstances._
import ST._

class STTest extends testlib.Spec {
  type ForallST[A] = Forall[({type λ[S] = ST[S, A]})#λ]

  "STRef" in {
    def e1[S] = for {
      x <- newVar[S](0)
      r <- x mod {_ + 1}
    } yield x
    def e2[S]: ST[S, Int] = for {
      x <- e1[S]
      r <- x.read
    } yield r
    runST(new ForallST[Int] { def apply[S] = e2[S] }) must be_===(1)
  }

  "STArray" in {
    def e1[S] = for {
      arr <- newArr[S, Boolean](3, true)
      _ <- arr.write(0, false)
      r <- arr.freeze
    } yield r
    runST(new ForallST[ImmutableArray[Boolean]] { def apply[S] = e1[S] }).toList must be_===(
      List(false, true, true))
  }
}
