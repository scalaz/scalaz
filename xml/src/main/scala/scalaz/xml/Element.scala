package scalaz
package xml

import CData._

sealed trait Element {
  /// elName
  val name: QName
  /// elAttribs
  val attribs: List[Attr]
  /// elContent
  val content: List[Content]
  /// elLine
  val line: Option[Line]

  import QName._
  import Element._
  import std.AllInstances._

  def sname: String =
    name.sname

  def setQname(a: QName): Element =
    element(a, attribs, content, line)

  def withQname(a: QName => QName): Element =
    element(a(name), attribs, content, line)

  def setAttribs(a: List[Attr]): Element =
    element(name, a, content, line)

  def ***(a: Attr*): Element =
    setAttribs(a.toList)

  def withAttribs(a: List[Attr] => List[Attr]): Element =
    element(name, a(attribs), content, line)

  def setContent(a: List[Content]): Element =
    element(name, attribs, a, line)

  def withContent(a: List[Content] => List[Content]): Element =
    element(name, attribs, a(content), line)

  def +:(a: Attr): Element =
    withAttribs(a :: _)

  def ++:(c: Content): Element =
    withContent(c :: _)

  def lineOr(l: => Line): Line =
    line getOrElse l

  def hasLine: Boolean =
    line.isDefined

  /// elChildren
  def children: List[Element] =
    content flatMap (_.elem)

  /// strContent
  def strContent: Str =
    Content.texts(content) flatMap (_.data)

  /// filterChildren
  def filterChildren(p: Element => Boolean): List[Element] =
    Content.elems(content) filter p

  /// filterChildrenName
  def filterChildrenQname(p: QName => Boolean): List[Element] =
    Content.elems(content) filter (e => p(e.name))

  /// filterChild
  def filterChild(p: Element => Boolean): Option[Element] =
    filterChildren(p).headOption

  /// filterChildName
  def filterChildQname(p: QName => Boolean): Option[Element] =
    filterChildrenQname(p).headOption

  /// filterElements
  def filterElements(p: Element => Boolean): List[Element] =
    if(p(this))
      List(this)
    else
      Content.elems(content) flatMap (_ filterElements p)

  /// filterElement
  def filterElement(p: Element => Boolean): Option[Element] =
    filterElements(p).headOption

  /// filterElementsName
  def filterElementsQname(p: QName => Boolean): List[Element] =
    filterElements(q => p(q.name))

  /// filterElementName
  def filterElementQname(p: QName => Boolean): Option[Element] =
    filterElementsQname(p).headOption

  /// findElements
  def findElements(n: QName): List[Element] =
    filterElementsQname(Equal[QName].equal(n, _))

  /// findElement
  def findElement(n: QName): Option[Element] =
    findElements(n).headOption

  /// findAttrBy
  def findAttrBy(p: QName => Boolean): Option[Str] =
    Attr.lookupBy(p, attribs)

  // alias for findAttrBy
  def !!(p: QName => Boolean): Option[Str] =
    findAttrBy(p)

  def ??(p: QName => Boolean): Boolean =
    (this !! p).isDefined

  def findAttrByOr(p: QName => Boolean, s: => Str): Str =
    findAttrBy(p) getOrElse s

  /// findAttr
  def findAttr(n: QName): Option[Str] =
    Attr.lookup(n, attribs)

  // alias for findAttr
  def !(n: QName): Option[Str] =
    findAttr(n)

  def ?(n: QName): Boolean =
    (this ! n).isDefined

  def findAttrOr(n: QName, s: => Str): Str =
    findAttr(n) getOrElse s

  import cursor._

  /// getTag
  def tag: Tag =
    Tag.tag(name, attribs, line)

  /// fromElement
  def toCursor: Cursor =
    Content.elem(this).toCursor

  def unary_~ : Content =
    Content.elem(this)

  def setName(a: Str): Element =
    withQname(_ setName a)

  def withName(a: Str => Str): Element =
    withQname(_ withName a)

  def setUri(a: Str): Element =
    withQname(_ setUri a)

  def setNoUri: Element =
    withQname(_ setNoUri)

  def withUri(a: Str => Str): Element =
    withQname(_ withUri a)

  def setPrefix(a: Str): Element =
    withQname(_ setPrefix a)

  def withPrefix(a: Str => Str): Element =
    withQname(_ withPrefix a)

  def setNoPrefix: Element =
    withQname(_ setNoPrefix)

  def filterAttrsBy(p: Attr => Boolean): Element =
    withAttribs(_ filter p)

  def filterAttrs(a: Attr): Element =
    filterAttrsBy(Equal[Attr].equal(a, _))

  def filterAttrsKeyBy(p: QName => Boolean): Element =
    filterAttrsBy(a => p(a.key))

  def filterAttrsKey(k: QName): Element =
    filterAttrsKeyBy(Equal[QName].equal(k, _))

  def filterAttrsKeyNameBy(p: Str => Boolean): Element =
    filterAttrsKeyBy(q => p(q.name))

  def filterAttrsKeyName(s: Str): Element =
    filterAttrsKeyNameBy(w => Equal[Str].equal(w, s))

  def filterAttrsKeySnameBy(p: String => Boolean): Element =
    filterAttrsKeyBy(q => p(q.sname))

  def filterAttrsKeySname(s: String): Element =
    filterAttrsKeySnameBy(_ == s)

  def removeAttrsBy(p: Attr => Boolean): Element =
    withAttribs(_ filter (a => !p(a)))

  def removeAttrs(a: Attr): Element =
    removeAttrsBy(Equal[Attr].equal(a, _))

  def removeAttrsKeyBy(p: QName => Boolean): Element =
    removeAttrsBy(a => p(a.key))

  def removeAttrsKey(k: QName): Element =
    removeAttrsKeyBy(Equal[QName].equal(k, _))

  def removeAttrsKeyNameBy(p: Str => Boolean): Element =
    removeAttrsKeyBy(q => p(q.name))

  def removeAttrsKeyName(s: Str): Element =
    removeAttrsKeyNameBy(w => Equal[Str].equal(w, s))

  def removeAttrsKeySnameBy(p: String => Boolean): Element =
    removeAttrsKeyBy(q => p(q.sname))

  def removeAttrsKeySname(s: String): Element =
    removeAttrsKeySnameBy(_ == s)

}

trait Elements {
  /// Element
  def element(name: QName, attribs: List[Attr] = Nil, content: List[Content] = Nil, line: Option[Line] = None): Element = {
    val n = name
    val a = attribs
    val c = content
    val l = line
    new Element {
      val name = n
      val attribs = a
      val content = c
      val line = l
    }
  }

  /// blank_element
  def blankElement: Element =
    element(QName.blankQname)

  import std.AllInstances._

  implicit val ElementShow: Show[Element] = new Show[Element] {
    override def shows(e: Element) =
      ("Element{name=" + Show[QName].shows(e.name) +
        ",attribs=" + Show[List[Attr]].shows(e.attribs) +
        ",content=" + Show[List[Content]].shows(e.content) +
        (e.line match {
          case None => ""
          case Some(l) => ",line=" + l
        }) +
        "}")
  }

  implicit val ElementEqual: Equal[Element] =
    Equal.equalBy(e => (e.name, e.attribs, e.content, e.line))

}

object Element extends Elements {

  import Lens._
  import StoreT._

  val nameElementL: Element @> QName =
    lens(x => store(x.name)(b => element(b, x.attribs, x.content, x.line)))

  val attribsElementL: Element @> List[Attr] =
    lens(x => store(x.attribs)(b => element(x.name, b, x.content, x.line)))

  val contentElementL: Element @> List[Content] =
    lens(x => store(x.content)(b => element(x.name, x.attribs, b, x.line)))

  val lineElementL: Element @> Option[Line] =
    lens(x => store(x.line)(b => element(x.name, x.attribs, x.content, b)))

}
