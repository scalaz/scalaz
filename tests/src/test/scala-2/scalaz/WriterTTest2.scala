package scalaz

import scalaz.std.AllInstances._

object WriterTTest2 {
  private def writerTUcompilationTest: Unit = {
    import syntax.either._
    val a: String \/ (Int, Boolean) = (1, true).right[String]
    WriterT.writerTU(a)
  }
}
