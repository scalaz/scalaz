package scalaz.example

import scalaz.syntax.FunctorV

object PartiallyApplied extends App {
  val f1: (String => Int) = _.length
  val f2: (String => String => Int) = x => y => x.length + y.length

  function1()

  def function1() {
    import scalaz._
    import instance.Function1._
    import syntax.Syntax.monad._

    f1.map(_ * 2)

    // This implicit view is used:
    {
      val functorV: FunctorV[({type f[a] = (String) => a})#f, Int] = functorBin[Function1, String, Int](f1)
      functorV.map(_ * 2)
    }

    f2.join
  }
}
