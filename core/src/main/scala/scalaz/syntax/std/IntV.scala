package scalaz
package syntax
package std

import scalaz.std.int

trait IntV extends SyntaxV[Int] {
  def ordering = int.ordering(self)
}

trait ToIntV {
  implicit def ToIntVFromInt(a: Int): IntV = new IntV {
    val self = a
  }
}
