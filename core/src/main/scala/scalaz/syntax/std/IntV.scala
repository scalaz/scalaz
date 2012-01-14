package scalaz
package syntax
package std

trait IntV extends SyntaxV[Int] {
  def ordering = Ordering.fromInt(self)
}

trait ToIntV {
  implicit def ToIntVFromInt(a: Int): IntV = new IntV {
    val self = a
  }
}
