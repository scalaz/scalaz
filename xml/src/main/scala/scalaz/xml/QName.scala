package scalaz
package xml


sealed trait QName {
  import QName._

  /// qName
  val name: Str
  /// qURI
  val uri: Option[Str]
  /// qPrefix
  val prefix: Option[Str]

  import CData._

  def sname: String =
    name.mkString

  def setName(a: Str): QName =
    qname(a, uri, prefix)

  def withName(a: Str => Str): QName =
    qname(a(name), uri, prefix)

  def setUri(s: Str): QName =
    qname(name, Some(s), prefix)

  def setNoUri: QName =
    qname(name, None, prefix)

  def withUri(a: Str => Str): QName =
    qname(name, uri map a, prefix)

  def uriOr(u: => Str): Str =
    uri getOrElse u

  def hasUri: Boolean =
    uri.isDefined

  def prefixOr(p: => Str): Str =
    prefix getOrElse p

  def hasPrefix: Boolean =
    prefix.isDefined

  def setPrefix(p: Str): QName =
    qname(name, uri, Some(p))

  def setNoPrefix: QName =
    qname(name, uri, None)

  def withPrefix(a: Str => Str): QName =
    qname(name, uri, prefix map a)

  /// findChildren
  def findChildren(e: Element): List[Element] =
    e filterChildren (q => implicitly[Equal[QName]].equal(q.name, this))

  /// findChild
  def findChild(e: Element): Option[Element] =
    findChildren(e).headOption

  /// lookupAttr
  def lookupAttr(as: List[Attr]): Option[Str] =
    Attr.lookup(this, as)

  def findAttr(e: Element): Option[Str] =
    e findAttr this

  def element(attribs: List[Attr] = Nil, content: List[Content] = Nil, line: Option[Line] = None): Element =
    Element.element(this, attribs, content, line)

  def contentElement(attribs: List[Attr] = Nil, content: List[Content] = Nil, line: Option[Line] = None): Content =
    Content.elem(element(attribs, content, line))

  def elem(attribs: Attr*): Element =
    element(attribs.toList)

}

trait QNames {
  type Str =
  List[Char]

  /// QName
  def qname(name: Str, uri: Option[Str] = None, prefix: Option[Str] = None): QName = {
    val n = name
    val u = uri
    val p = prefix
    new QName {
      val name = n
      val uri = u
      val prefix = p
    }
  }

  def qnames(name: String, uri: Option[Str] = None, prefix: Option[Str] = None): QName =
    qname(name.toList, uri, prefix)

  /// blank_name
  def blankQname: QName =
    qname(Nil)

  import std.AllInstances._

  implicit val QNameEqual: Equal[QName] = new Equal[QName] {
    def equal(q1: QName, q2: QName) =
      Equal.equalBy[QName, (Str, Option[Str], Option[Str])](q => (q.name, q.uri, q.prefix)).equal(q1, q2)
  }

  implicit val QNameOrder: Order[QName] = new Order[QName] {
    def order(q1: QName, q2: QName) =
      (q1.uri, q2.uri) match {
        case (None, None) => implicitly[Order[Option[Str]]].order(q1.prefix, q2.prefix)
        case (u1, u2) => implicitly[Order[Option[Str]]].order(u1, u2)
      }
  }

  implicit val QNameShow: Show[QName] = new Show[QName] {
    def show(q: QName) =
      ("QName{name=" + q.name.mkString + (q.uri match {
        case None => ""
        case Some(u) => ",uri=" + u.mkString
      }) + (q.prefix match {
        case None => ""
        case Some(p) => ",prefix=" + p.mkString
      }) + "}").toList
  }

}

object QName extends QNames {

  import Lens._
  import CoStateT._

  val nameQNameL: QName @-@ Str =
    lens(x => coState(b => qname(b, x.uri, x.prefix), x.name))

  val uriQNameL: QName @-@ Option[Str] =
    lens(x => coState(b => qname(x.name, b, x.prefix), x.uri))

  val prefixQNameL: QName @-@ Option[Str] =
    lens(x => coState(b => qname(x.name, x.uri, b), x.prefix))

}
