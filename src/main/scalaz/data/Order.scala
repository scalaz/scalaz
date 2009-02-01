package scalaz.data

sealed trait Order[-A] {
  def apply(a1: A, a2: A): Ordering
}
