package scalaz
package xml
package cursor

import CData._

sealed trait Tag {
  val name: QName
  val attribs: List[Attr]
  val line: Option[Line]

  import Element._

  /// setTag
  def setTag(e: Element): Element =
    fromTag(e.content)

  /// fromTag
  def fromTag(cs: List[Content]): Element =
    element(
      name = this.name
    , attribs = this.attribs
    , line = this.line
    , content = cs
    )

}

trait Tags {
  def tag(name: QName, attribs: List[Attr] = Nil, line: Option[Line] = None): Tag = {
    val n = name
    val a = attribs
    val l = line
    new Tag {
      val name = n
      val attribs = a
      val line = l
    }
  }

  import std.AllInstances._

  implicit val TagShow: Show[Tag] = new Show[Tag] {
    override def shows(t: Tag) =
      ("Tag{name=" + Show[QName].shows(t.name) + ",attribs=" + Show[List[Attr]].shows(t.attribs) + (t.line match {
        case None => ""
        case Some(l) => ",line=" + l
      }) + "}")
  }

  implicit val TagEqual: Equal[Tag] =
    Equal.equalBy(t => (t.name, t.attribs, t.line))

}

object Tag extends Tags {

  import Lens._
  import StoreT._

  val nameTagL: Tag @> QName =
    lens(x => store(x.name)(b => tag(b, x.attribs, x.line)))

  val attribsTagL: Tag @> List[Attr] =
    lens(x => store(x.attribs)(b => tag(x.name, b, x.line)))

  val lineTagL: Tag @> Option[Line] =
    lens(x => store(x.line)(b => tag(x.name, x.attribs, b)))

}
