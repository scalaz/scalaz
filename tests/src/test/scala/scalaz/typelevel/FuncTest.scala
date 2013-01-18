package scalaz
package typelevel

class FuncTest extends Spec {
  import std.AllInstances._
  import scalaz.syntax.typelevel.hlist._

  val f = AppFuncU { (x: Int) => List(x + 1) }
  val g = AppFuncU { (x: Int) => List(x, 5) }

  "andThenA" in {
    val test = f @>>> g
    test.runA(1) must be_===(List(List(2, 5))) 
  }

  "composeA" in {
    val test = f <<<@ g
    test.runA(1) must be_===(List(List(2), List(6))) 
  }

  "productA" in {
    val test = f @&&& g
    val result = test.runA(1)
    result match {
      case x :: y :: HNil =>
        { (x: List[Int]) must be_===(List(2)) } and
        { (y: List[Int]) must be_===(List(1, 5)) }
    }
  }

  "consA" in {
    val test = f :: g :: AppFunc.HNil
    val result = test.runA(1)
    result match {
      case x :: y :: HNil =>
        { (x: List[Int]) must be_===(List(2)) } and
        { (y: List[Int]) must be_===(List(1, 5)) }
    }
  }

  "hlistfunc" in {
    val test = List(1, 2, 3).:\(AppFunc.HNil[Int, Int]: HListFunc[scalaz.Applicative, Int, Int]) ({ (n, acc) => 
      AppFuncU { (x: Int) => (Some(x + n): Option[Int]) } :: acc })
    val result = test.runA(1)
    result match {
      case Some(x: Int) :: Some(y: Int) :: Some(z: Int) :: HNil =>
        { x must be_===(2) } and
        { y must be_===(3) } and
        { z must be_===(4) }   
    }
  }

  "traverse" in {
    val test = List(1, 2, 3).:\(AppFunc.HNil[Int, Int]: HListFunc[scalaz.Applicative, Int, Int]) ({ (n, acc) => 
      AppFuncU { (x: Int) => (Some(x + n): Option[Int]) } :: acc })
    val result = test traverse List(1, 2)
    result match {
      case Some(List(x0: Int, x1: Int)) ::
           Some(List(y0: Int, y1: Int)) ::
           Some(List(z0: Int, z1: Int)) :: HNil =>
        { List(x0, x1) must be_===(List(2, 3)) } and
        { List(y0, y1) must be_===(List(3, 4)) } and
        { List(z0, z1) must be_===(List(4, 5)) } 
    }
  }
}
