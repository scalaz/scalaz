package scalaz
package xml

import QName._

sealed trait Content {
  def fold[X](
    elem: Element => X
  , text: CData => X
  , cref: Str => X
  , comment: Str => X
  ): X =
    this match {
      case Elem(e) => elem(e)
      case Text(d) => text(d)
      case CRef(s) => cref(s)
      case Comment(s) => comment(s)
    }

  def elem: Option[Element] =
    fold(Some(_), _ => None, _ => None, _ => None)

  def elemOr(e: => Element): Element =
    elem getOrElse e

  def isElem: Boolean =
    elem.isDefined

  def text: Option[CData] =
    fold(_ => None, Some(_), _ => None, _ => None)

  def textOr(d: => CData): CData =
    text getOrElse d

  def isText: Boolean =
    text.isDefined

  def cref: Option[Str] =
    fold(_ => None, _ => None, Some(_), _ => None)

  def crefOr(s: => Str): Str =
    cref getOrElse s

  def isCref: Boolean =
    cref.isDefined

  def comment: Option[Str] =
    fold(_ => None, _ => None, _ => None, Some(_))

  def commentOr(s: => Str): Str =
    comment getOrElse s

  def isComment: Boolean =
    comment.isDefined

  def usingElem(k: Element => Element): Content =
    fold(
      w => Content.elem(k(w))
    , z => Content.text(z)
    , z => Content.cref(z)
    , z => Content.comment(z)
    )

  def usingText(k: CData => CData): Content =
    fold(
      z => Content.elem(z)
    , w => Content.text(k(w))
    , z => Content.cref(z)
    , z => Content.comment(z)
    )

  def usingCref(k: Str => Str): Content =
    fold(
      z => Content.elem(z)
    , z => Content.text(z)
    , w => Content.cref(k(w))
    , z => Content.comment(z)
    )

  def usingComment(k: Str => Str): Content =
    fold(
      z => Content.elem(z)
    , z => Content.text(z)
    , z => Content.cref(z)
    , w => Content.comment(k(w))
    )

  import cursor._

  /// fromContent
  def toCursor: Cursor =
    Cursor.cursor(
      current = this
    )

  def unary_+ : Cursor =
    toCursor

  def walk(k: Cursor => Content): Content =
    -(+this walk k)

}
private case class Elem(e: Element) extends Content
private case class Text(d: CData) extends Content
private case class CRef(s: Str) extends Content
private case class Comment(s: Str) extends Content

trait Contents {
  type Forest = List[Content]

  def elem(e: Element): Content =
    Elem(e)

  def text(d: CData): Content =
    Text(d)

  def cref(s: Str): Content =
    CRef(s)

  def comment(s: Str): Content =
    Comment(s)

  /// onlyElems
  def elems(c: List[Content]): List[Element] =
    c flatMap (_.elem)

  /// onlyText
  def texts(c: List[Content]): List[CData] =
    c flatMap (_.text)

  import cursor._

  /// fromForest
  def forestToCursor(forest: Forest): Option[Cursor] =
    forest match {
      case t :: ts => Some(Cursor.cursor(current = t, rights = ts))
      case Nil => None
    }

  def forestToCursorOr(forest: Forest, c: => Cursor): Cursor =
    forestToCursor(forest) getOrElse c

  import std.AllInstances._


  implicit val ContentShow: Show[Content] = new Show[Content] {
    def show(c: Content) =
      ("Content{" + (c match {
        case Elem(e) => "Elem(" + implicitly[Show[Element]].shows(e) + ")"
        case Text(d) => "Text(" + implicitly[Show[CData]].shows(d) + ")"
        case CRef(s) => "CRef(" + s.mkString + ")"
        case Comment(s) => "Comment(" + s.mkString + ")"
      }) + "}").toList
  }

  implicit val ContentEqual: Equal[Content] = new Equal[Content] {
    def equal(a1: Content, a2: Content) =
      (a1, a2) match {
        case (Elem(e), Elem(f)) => implicitly[Equal[Element]].equal(e, f)
        case (Text(d), Text(e)) => implicitly[Equal[CData]].equal(d, e)
        case (CRef(s), CRef(t)) => implicitly[Equal[Str]].equal(s, t)
        case (Comment(s), Comment(t)) => implicitly[Equal[Str]].equal(s, t)
        case (_, _) => false
      }
  }

}

object Content extends Contents {

  import PLens._
  import CoStateT._

  val elemContentPL: Content @-? Element =
    plens(_.elem map (e => coState(elem(_), e)))

  val textContentPL: Content @-? CData =
    plens(_.text map (c => coState(text(_), c)))

  val crefContentPL: Content @-? Str =
    plens(_.cref map (e => coState(cref(_), e)))

  val commentContentPL: Content @-? Str =
    plens(_.comment map (e => coState(comment(_), e)))

}
