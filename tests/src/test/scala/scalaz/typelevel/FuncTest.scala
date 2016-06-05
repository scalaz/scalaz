package scalaz
package typelevel
import org.scalacheck.Prop.forAll

object FuncTest extends SpecLite {
  import std.AllInstances._
  import scalaz.syntax.typelevel.hlist._

  "andThenA" in {
    val f = AppFuncU { (x: Int) => List(x + 1) }
    val g = AppFuncU { (x: Int) => List(x, 5) }

    val test = f @>>> g
    test.runA(1) must_===(List(List(2, 5)))
  }

  "composeA" in {
    val f = AppFuncU { (x: Int) => List(x + 1) }
    val g = AppFuncU { (x: Int) => List(x, 5) }

    val test = f <<<@ g
    test.runA(1) must_===(List(List(2), List(6)))
  }

  "productA" in {
    val f = AppFuncU { (x: Int) => List(x + 1) }
    val g = AppFuncU { (x: Int) => List(x, 5) }

    val test = f @&&& g
    val result = test.runA(1)
    result match {
      case x :: y :: HNil =>
        { (x: List[Int]) must_===(List(2)) } and
        { (y: List[Int]) must_===(List(1, 5)) }
    }
  }

  "consA" in {
    val f = AppFuncU { (x: Int) => List(x + 1) }
    val g = AppFuncU { (x: Int) => List(x, 5) }

    val test = f :: g :: AppFunc.HNil
    val result = test.runA(1)
    result match {
      case x :: y :: HNil =>
        { (x: List[Int]) must_===(List(2)) } and
        { (y: List[Int]) must_===(List(1, 5)) }
    }
  }

  "hlistfunc" in {
    val test = List(1, 2, 3).:\(AppFunc.HNil[Int, Int]: HListFunc[scalaz.Applicative, Int, Int]) ({ (n, acc) =>
      AppFuncU { (x: Int) => (Some(x + n): Option[Int]) } :: acc })
    val result = test.runA(1)
    result match {
      case Some(x: Int) :: Some(y: Int) :: Some(z: Int) :: HNil =>
        { x must_===(2) } and
        { y must_===(3) } and
        { z must_===(4) }
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
        { List(x0, x1) must_===(List(2, 3)) } and
        { List(y0, y1) must_===(List(3, 4)) } and
        { List(z0, z1) must_===(List(4, 5)) }
    }
  }
}
