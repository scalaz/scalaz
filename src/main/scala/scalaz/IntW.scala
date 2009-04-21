package scalaz

sealed trait IntW {
  val value: Int

  def |*| = IntMultiplication.multiplication(value)
}

object IntW {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }

  implicit def IntFrom(n: IntW) = n.value
}
