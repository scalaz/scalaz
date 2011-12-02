package scalaz.example

import scalaz._
import syntax.monad._
import scalaz.ContT
import ContT._
import Id._

object ContTUsage extends App {
  def calcLength[R](in:Seq[_]):ContT[Id,R,Int] = constT(in.length)

  def foo[R](s:Boolean,alt: => Id[R], alt2: => Id[R]):ContT[Id,R,Int] = for {
    x <- calcLength[R](List(1,2,3))
    z <- callCCT[Id,R,Int,Int](k => if (s) k(55) else ContT(_ => alt2))
    y <- calcLength[R](List(1,2,3,4))
  } yield (z)

  runContT[Id,Unit,Int](foo(true,println("Woo hoo!"),println("WTF?")))(println _)
  runContT[Id,Unit,Int](foo(false,println("Woo hoo!"),println("WTF?")))(println _)
  println("success")
}
