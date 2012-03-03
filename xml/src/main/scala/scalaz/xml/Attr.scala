package scalaz
package xml

import QName._

sealed trait Attr {
  /// attrKey
  val key: QName
  /// attrVal
  val value: Str
}

trait Attrs {
  /// Attr
  def attr(key: QName, value: Str): Attr = {
    val k = key
    val v = value
    new Attr {
      val key = k
      val value = v
    }
  }

  def attrs(key: String, value: String): Attr =
    attr(qnames(key), value.toList)

  /// lookupAttrBy
  def lookupBy(p: QName => Boolean, as: List[Attr]): Option[Str] =
    as find (a => p(a.key)) map (_.value)

  /// lookupAttr
  def lookup(n: QName, as: List[Attr]): Option[Str] =
    lookupBy(implicitly[Equal[QName]].equal(n, _), as)

  import std.AllInstances._

  implicit val AttrEqual: Equal[Attr] =
    Equal.equalBy[Attr, (QName, Str)](c => (c.key, c.value))

  implicit val AttrOrder: Order[Attr] =
    Order.orderBy[Attr, (QName, Str)](c => (c.key, c.value))

  implicit val AttrShow: Show[Attr] = new Show[Attr] {
    def show(c: Attr) =
      ("Attr{key=" + implicitly[Show[QName]].shows(c.key) + ",value=" + c.value.mkString + "}").toList
  }

}

object Attr extends Attrs {

  import Lens._
  import CoStateT._

  val keyAttrL: Attr @-@ QName =
    lens(x => coState(b => attr(b, x.value), x.key))

  val valueAttrL: Attr @-@ Str =
    lens(x => coState(b => attr(x.key, b), x.value))

}
