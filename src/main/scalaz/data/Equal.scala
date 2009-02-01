package scalaz.data

sealed trait Equal[-A] {
  def apply(a1: A, a2: A): Boolean  
}
