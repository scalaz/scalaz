package scalaz.data

sealed trait State[S, +A] {
  def apply(s: S): (S, A)  
}
