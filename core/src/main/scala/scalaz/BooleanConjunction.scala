package scalaz

sealed trait BooleanConjunction {
  val value: Boolean
}

trait BooleanConjunctions {
  def conjunction(b: Boolean) = new BooleanConjunction {
    val value = b
  }

  implicit def BooleanConjunctionFrom(b: BooleanConjunction): Boolean = b.value
}
