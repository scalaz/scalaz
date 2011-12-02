package scalaz.example

import scalaz._
import syntax.monad._
import scalaz.ContT
import ContT._
import Id._

object ContTUsage {

  def test1 {
    def calcLength[R](in:Seq[_]):ContT[Id,R,Int] = constT(in.length)

    def foo[R](s:Boolean,alt2: => Id[R]):ContT[Id,R,Int] = for {
      x <- calcLength[R](List(1,2,3))
      z <- callCCT[Id,R,Int,Int](k => if (s) k(55) else exitCCT(alt2))
      y <- calcLength[R](List(1,2,3,4))
    } yield (x + y + z)

    assert(runContT[Id,String,Int](foo(true,"WTF?"))(_.toString) == "62")
    assert(runContT[Id,String,Int](foo(false,"WTF?"))(_.toString) == "WTF?")
    println("success")
  }

  def test2 {
    def foo[R](a:Int):ContT[Id,R,Int] = constT(a + 7)
    def bar[R](alt:Int => Id[R])(a:Int):ContT[Id,R,Int] = callCCT[Id,R,Int,Int](k => if (a > 20) exitCCT(alt(a)) else k(a * 7))
    def baz[R](a:Int,alt:Int => Id[R]):ContT[Id,R,Int] = foo[R](a) >>= (x => bar[R](alt)(x))

    assert(runContT[Id,String,Int](baz(10,((x:Int) => "%d>20 -> too big".format(x))))(_.toString) == "119")
    assert(runContT[Id,String,Int](baz(20,((x:Int) => "%d>20 -> too big".format(x))))(_.toString) == "27>20 -> too big")
    println("success")
  }

  def main(args:Array[String]) {
    test1
    test2
  }
}