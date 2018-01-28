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

  final class CordRepr(val under: AnyRef, val length: Int)
  final class Concat(val left: AnyRef, val right: AnyRef)

  def apply(string: String): Cord = new CordRepr(string, string.length)

  def concat(left: Cord, right: Cord): Cord = new CordRepr(new Concat(left.under, right.under), left.length + right.length)
  def cons(left: String, right: Cord): Cord = new CordRepr(new Concat(left, right.under), left.length + right.length)
  def snoc(left: Cord, right: String): Cord = new CordRepr(new Concat(left.under, right), left.length + right.length)
  val empty: Cord = new CordRepr("", 0)

  def length(cord: Cord): Int = cord.length

  def fold(cord: Cord): String = {
    val sb = new StringBuilder(cord.length)
    unsafeAppendTo(cord, sb)
    sb.toString()
  }

  def unsafeAppendToH(rights: java.util.ArrayDeque[AnyRef], cord: Cord, builder: StringBuilder): Unit = {
    var current = cord.under
    while (current != null) current match {
      case s: String =>
        builder.append(s)
        if (rights.size() > 0) {
          current = rights.pop()
        } else {
          current = null
        }
      case c: Concat =>
        current = c.left
        rights.push(c.right)
    }
  }

  def unsafeAppendTo(cord: Cord, builder: StringBuilder): Unit = {
    val rights = new java.util.ArrayDeque[AnyRef](64)
    unsafeAppendToH(rights, cord, builder)
  }
}
