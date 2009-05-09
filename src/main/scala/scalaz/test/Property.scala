package scalaz.test

sealed trait Property {
  val gen: Gen[Status]
}

object Property {
  def property(g: Gen[Status]) = new Property {
    val gen = g
  }
}