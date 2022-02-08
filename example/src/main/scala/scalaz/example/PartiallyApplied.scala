package scalaz.example

object PartiallyApplied {
  val f1: (String => Int) = _.length
  val f2: (String => String => Int) = x => y => x.length + y.length

  def main(args: Array[String]): Unit = {
    function1()
  }

  def function1(): Unit = {
    import scalaz._
    import std.function._
    import syntax.monad._

    // uses implicit view ToFunctorVFromBin
    f1.map(_ * 2)
    f2.join
  }
}
