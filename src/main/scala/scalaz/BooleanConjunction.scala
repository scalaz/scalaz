package scalaz

sealed trait BooleanConjunction {
  val value: Boolean
}

object BooleanConjunction {
  def booleanConjunction(b: Boolean) = new BooleanConjunction {
    val value = b
  }

  implicit def BooleanConjunctionFrom(b: BooleanConjunction) = b.value
}
