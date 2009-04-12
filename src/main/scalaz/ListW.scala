package scalaz

sealed trait ListW[+A] {
  val value: List[A]
}

object ListW {
  implicit def ListTo[A](as: List[A]) = new ListW[A] {
    val value = as
  }

  implicit def ListFrom[A](as: ListW[A]) = as.value
}
