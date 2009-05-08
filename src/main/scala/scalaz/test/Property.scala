package scalaz.test

sealed trait Property {
  val gen: Gen[Result]
}

object Property {
  def property(g: Gen[Result]) = new Property {
    val gen = g
  }
}