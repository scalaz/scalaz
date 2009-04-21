package scalaz

sealed trait IntW {
  val value: Int
}

object IntW {
  implicit def IntTo(n: Int): IntW = new IntW {
    val value = n
  }

  implicit def IntFrom(n: IntW) = n.value
}
