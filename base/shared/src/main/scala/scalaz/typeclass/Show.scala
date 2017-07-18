package scalaz

abstract class Show[A] {
  def show(a: A): String
}
