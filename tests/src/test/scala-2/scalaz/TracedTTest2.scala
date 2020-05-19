package scalaz

object TracedTTest2 {
  def compilationTestTracedTU(): Unit = {
    import scalaz.syntax.either._
    import scalaz.std.function._

    val a: Int \/ (Byte => String) = 1.left[Byte => String]
    TracedT.tracedTU(a)
  }
}
