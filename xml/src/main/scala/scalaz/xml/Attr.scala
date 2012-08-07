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
    lookupBy(Equal[QName].equal(n, _), as)

  import std.AllInstances._

  implicit val AttrEqual: Equal[Attr] =
    Equal.equalBy[Attr, (QName, Str)](c => (c.key, c.value))

  implicit val AttrOrder: Order[Attr] =
    Order.orderBy[Attr, (QName, Str)](c => (c.key, c.value))

  implicit val AttrShow: Show[Attr] = new Show[Attr] {
    override def shows(c: Attr) =
      ("Attr{key=" + Show[QName].shows(c.key) + ",value=" + c.value.mkString + "}")
  }

}

object Attr extends Attrs {

  import Lens._
  import StoreT._

  val keyAttrL: Attr @> QName =
    lens(x => store(x.key)(b => attr(b, x.value)))

  val valueAttrL: Attr @> Str =
    lens(x => store(x.value)(b => attr(x.key, b)))

}
