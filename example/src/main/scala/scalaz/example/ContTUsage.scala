package scalaz.example

import scalaz._
import syntax.monad._
import scalaz.ContT
import ContT._
import Id._

object ContTUsage extends App {
  def calcLength[R](in:Seq[_]):ContT[Id,R,Int] = constT(in.length)

  def foo[R](s:Boolean,alt2: => Id[R]):ContT[Id,R,Int] = for {
    x <- calcLength[R](List(1,2,3))
    z <- callCCT[Id,R,Int,Int](k => if (s) k(55) else exitT(alt2))
    y <- calcLength[R](List(1,2,3,4))
  } yield (x + y + z)

  runContT[Id,Unit,Int](foo(true,println("WTF?")))(println _)
  runContT[Id,Unit,Int](foo(false,println("WTF?")))(println _)
  println("success")
}

object ContTUsageBind extends App {
  def foo[R](a:Int):ContT[Id,R,Int] = constT(a + 7)
  def bar[R](alt:Int => Id[R])(a:Int):ContT[Id,R,Int] = callCCT[Id,R,Int,Int](k => if (a > 20) exitT(alt(a)) else k(a * 7))
  def baz[R](a:Int,alt:Int => Id[R]):ContT[Id,R,Int] = foo[R](a) >>= (x => bar[R](alt)(x))

  runContT[Id,Unit,Int](baz(10,((x:Int) => println("%d>20 -> too big".format(x)))))(println _)
  runContT[Id,Unit,Int](baz(20,((x:Int) => println("%d>20 -> too big".format(x)))))(println _)
  println("success")
}
