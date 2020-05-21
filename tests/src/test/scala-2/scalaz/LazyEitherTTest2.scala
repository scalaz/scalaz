package scalaz


object LazyEitherTTest2 {

  private def lazyEitherTUcompilationTest: Unit = {
    val a: String \/ LazyEither[Int, Boolean] = null
    LazyEitherT.lazyEitherTU(a)
  }

}
