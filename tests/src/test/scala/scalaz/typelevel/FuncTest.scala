package scalaz
package typelevel

class FuncTest extends Spec {
  import org.specs2.matcher._
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
        (x must_== List(2)) and
        (y must_== List(1, 5))
    }
  }

  "consA" in {
    val test = f :: g :: AppFunc.HNil
    val result = test.runA(1)
    result match {
      case x :: y :: HNil =>
        (x must_== List(2)) and
        (y must_== List(1, 5))
    }
  }

  "hlistfunc" in {
    val test = List(1, 2, 3).:\(AppFunc.HNil[Int, Int]) ({ (n, acc) => 
      AppFuncU { (x: Int) => (Some(x + n): Option[Int]) } :: acc })
    val result = test.runA(1)
    result match {
      case x :: y :: z :: HNil =>
        (x must_== Some(2)) and
        (y must_== Some(3)) and
        (z must_== Some(4))   
    }
  }

  "traverse" in {
    val test = List(1, 2, 3).:\(AppFunc.HNil[Int, Int]) ({ (n, acc) => 
      AppFuncU { (x: Int) => (Some(x + n): Option[Int]) } :: acc })
    val result = test traverse List(1, 2)
    result match {
      case x :: y :: z :: HNil =>
        (x must_== Some(List(2, 3))) and
        (y must_== Some(List(3, 4))) and
        (z must_== Some(List(4, 5)))   
    }
  }
}
