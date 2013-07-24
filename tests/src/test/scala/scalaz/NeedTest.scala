package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class NeedTest extends Spec {
  // TODO check distributive laws

  checkAll("Value", monad.laws[Value])
  checkAll("Value", comonad.laws[Value])

  checkAll("Name", monad.laws[Name])
  checkAll("Name", comonad.laws[Name])

  checkAll("Need", monad.laws[Need])
  checkAll("Need", comonad.laws[Need])

  "Need" should {
    "clear the Function0 reference" in {
      @volatile var flag = false
      val method = Need.getClass.getMethod("apply", classOf[Function0[_]])
      val need = method.invoke(
        Need,
        new runtime.AbstractFunction0[String]{
          override def finalize = {flag = true}
          override def apply = ""
        }
      ).asInstanceOf[Need[String]]

      flag must_== false
      print(need.value)
      System.gc()
      System.runFinalization()
      flag must_== true
    }
  }

}
