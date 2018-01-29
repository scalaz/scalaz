package scalaz
package data

trait CordModule {
  type Cord >: Null <: AnyRef

  def apply(string: String): Cord

  def concat(left: Cord, right: Cord): Cord
  def cons(left: String, right: Cord): Cord
  def snoc(left: Cord, right: String): Cord
  def empty: Cord

  def length(cord: Cord): Int

  def unsafeAppendTo(cord: Cord, builder: StringBuilder): Unit

  def fold(cord: Cord): String
}

object CordImpl extends CordModule {
  // Cord = (String | Concat, Int)
  // Concat = (String | Concat, String | Concat)

  type Cord = CordRepr

  final class CordRepr(val under: AnyRef, val length: Int, val depth: Int)
  final class Concat(val left: AnyRef, val right: AnyRef)

  def apply(string: String): Cord = new CordRepr(string, string.length, 1)

  def concat(left: Cord, right: Cord): Cord =
    new CordRepr(new Concat(left.under, right.under), left.length + right.length, Math.max(left.depth + 1, right.depth))
  def cons(left: String, right: Cord): Cord =
    new CordRepr(new Concat(left, right.under), left.length + right.length, right.depth)
  def snoc(left: Cord, right: String): Cord =
    new CordRepr(new Concat(left.under, right), left.length + right.length, left.depth + 1)
  val empty: Cord = new CordRepr("", 0, 1)

  def length(cord: Cord): Int = cord.length

  def fold(cord: Cord): String = {
    val sb = new StringBuilder(cord.length)
    unsafeAppendTo(cord, sb)
    sb.toString()
  }

  def unsafeAppendToH(rights: Array[AnyRef], cord: Cord, builder: StringBuilder): Unit = {
    var current = cord.under
    var stackPtr = 0
    while (current != null) current match {
      case s: String =>
        builder.append(s)
        if (stackPtr > 0) {
          stackPtr = stackPtr - 1
          current = rights(stackPtr)
        } else {
          current = null
        }
      case c: Concat =>
        current = c.left
        rights(stackPtr) = c.right
        stackPtr = stackPtr + 1
    }
  }

  def unsafeAppendTo(cord: Cord, builder: StringBuilder): Unit = {
    val rights = new Array[AnyRef](cord.depth)
    unsafeAppendToH(rights, cord, builder)
  }
}
