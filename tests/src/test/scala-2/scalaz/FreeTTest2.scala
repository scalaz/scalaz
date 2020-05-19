package scalaz

object FreeTTest2 {
  private def compilationTest = {
    val a: String \/ Int = \/-(42)
    val b: FreeT[Maybe, \/[String, *], Int] = FreeT.liftMU[Maybe, String \/ Int](a)
  }
}
