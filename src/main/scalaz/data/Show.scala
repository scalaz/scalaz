package scalaz.data

sealed trait Show[-A] {
  def apply(a: A): String  
}
