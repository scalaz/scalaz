package scalaz

trait Dual[A] {
  val value : A
}

trait Duals {
  implicit def DualTo[A](a: A): Dual[A] = new Dual[A] {
    val value = a
  }

  implicit def DualFrom[A](d: Dual[A]): A = d.value
}
