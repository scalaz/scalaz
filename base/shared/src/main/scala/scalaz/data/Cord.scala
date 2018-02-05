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

  def unsafeAppendToH(rights: Array[AnyRef], out: Array[Char], cord: AnyRef): Unit = {
    var current = cord
    var stackPtr = 0
    var outputPtr = 0
    while (current != null) {
      if (current.getClass == classOf[String]) {
        val s = current.asInstanceOf[String]
        s.getChars(0, s.length, out, outputPtr)
        outputPtr += s.length
        if (stackPtr > 0) {
          stackPtr = stackPtr - 1
          current = rights(stackPtr)
        } else {
          current = null
        }
      } else {
        val c = current.asInstanceOf[Concat]
        current = c.left
        rights(stackPtr) = c.right
        stackPtr = stackPtr + 1
      }
    }
  }

  def fold(cord: Cord): String = {
    val rights = new Array[AnyRef](cord.depth)
    val out = new Array[Char](cord.length)
    unsafeAppendToH(rights, out, cord.under)
    new String(out)
  }
}
