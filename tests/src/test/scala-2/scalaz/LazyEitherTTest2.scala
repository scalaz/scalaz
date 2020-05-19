package scalaz

import std.AllInstances._

object LazyEitherTTest2 {

  private def lazyEitherTUcompilationTest: Unit = {
    val a: String \/ LazyEither[Int, Boolean] = null
    LazyEitherT.lazyEitherTU(a)
  }

}
