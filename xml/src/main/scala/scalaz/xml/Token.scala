package scalaz
package xml

import CData._
import QName._

sealed trait Token {
  def fold[X](
    start: Line => QName => List[Attr] => Boolean => X
  , end: Line => QName => X
  , cref: Str => X
  , text: CData => X
  , comment: Str => X
  ): X =
    this match {
      case StartToken(l, q, a, e) => start(l)(q)(a)(e)
      case EndToken(l, q) => end(l)(q)
      case CRefToken(s) => cref(s)
      case TextToken(d) => text(d)
      case CommentToken(s) => comment(s)
    }

  def isStart: Boolean =
    fold(_ => _ => _ => _ => true, _ => _ => false, _ => false, _ => false, _ => false)

  def isEnd: Boolean =
    fold(_ => _ => _ => _ => false, _ => _ => true, _ => false, _ => false, _ => false)

  def isCref: Boolean =
    fold(_ => _ => _ => _ => false, _ => _ => false, _ => true, _ => false, _ => false)

  def isText: Boolean =
    fold(_ => _ => _ => _ => false, _ => _ => false, _ => false, _ => true, _ => false)

  def isComment: Boolean =
    fold(_ => _ => _ => _ => false, _ => _ => false, _ => false, _ => false, _ => true)

}
private case class StartToken(l: Line, q: QName, a: List[Attr], e: Boolean) extends Token
private case class EndToken(l: Line, q: QName) extends Token
private case class CRefToken(s: Str) extends Token
private case class TextToken(d: CData) extends Token
private case class CommentToken(s: Str) extends Token

trait Tokens {
  type LChar =
  (Line, Char)

  type LStr =
  List[LChar]

  def startToken(l: Line, q: QName, a: List[Attr], e: Boolean): Token =
    StartToken(l, q, a, e)

  def endToken(l: Line, q: QName): Token =
    EndToken(l, q)

  def crefToken(s: Str): Token =
    CRefToken(s)

  def textToken(d: CData): Token =
    TextToken(d)

  def commentToken(s: Str): Token =
    CommentToken(s)

  import std.AllInstances._

  implicit def TokenShow: Show[Token] = new Show[Token] {
    def show(t: Token) =
      t.fold(
        start = l => q => a => e =>
          "StartToken{line=" + l + ",qname=" + Show[QName].shows(q) + ",attributes=" + Show[List[Attr]].shows(a) + ",empty=" + e + "}"
        , end = l => q =>
          "EndToken{line=" + l + ",qname=" + Show[QName].shows(q) + "}"
        , cref = s =>
          "CRefToken{str=" + Show[Str].shows(s) + "}"
        , text = d =>
          "CDataToken{data=" + Show[CData].shows(d) + "}"
        , comment = s =>
          "CommentToken{str=" + s.mkString + "}"
      ).toList
  }

  /// break'
  private[xml] def breakk[A, B](p: A => Boolean, xs: List[(B, A)]): (List[A], List[(B, A)]) = {
    val (as, bs) = breakn(p, xs)
    (as, bs drop 1)
  }

  /// breakn
  private[xml] def breakn[A, B](p: A => Boolean, xs: List[(B, A)]): (List[A], List[(B, A)]) = {
    val (as, bs) = xs span (w => !p(w._2))
    (as map (_._2), bs)
  }

  /// string
  private[xml] def string(s: LStr): (Str, LStr) =
    s match {
      case (_, '"') :: cs => breakk((_: Char) == '"', cs)
      case (_, '\'') :: cs => breakk((_: Char) == '\'', cs)
      case _ => breakn((x: Char) => x.isWhitespace || x == '>' || x == '/', s)
    }

  /// dropSpace
  private[xml] def dropSpace(s: LStr): LStr =
    s dropWhile (_._2.isWhitespace)

  /// attr_val
  private[xml] def attrVal(s: LStr): (Str, LStr) =
    s match {
      case (_, '=') :: cs => string(dropSpace(cs))
      case _ => (Nil, s)
    }

  /// qualName
  private[xml] def qualName(s: LStr): (QName, LStr) = {
    val (as, bs) = breakn((c: Char) => c.isWhitespace || c == '=' || c == '>' || c == '/', s)
    val (q, n) = as span (_ != ':') match {
      case (q1, _:: n1) => (Some(q1), n1)
      case _ => (None, as)
    }
    (qname(
        name = n
      , uri = None
      , prefix = q
      ), bs)
  }

  // why can't I import std.function._? compiler -> StackOverflowError
  private[xml] implicit val Function0Functor: Functor[Function0] = new Functor[Function0] {
    def map[A, B](fa: Function0[A])(f: A => B) =
      () => f(fa())
  }

  /// attrib
  private[xml] def attrib(s: LStr): (Attr, LStr) = {
    val (ks, cs1) = qualName(s)
    val (vs, cs2) = attrVal(dropSpace(cs1))
    (Attr.attr(
        key = ks
      , value = Txt.decodeAttr(vs)
      ), dropSpace(cs2))
  }

  //// trampolined
  import Free._

  /// parse
  def parse(ts: List[Token]): List[Content] = {
    def tparse(ts: List[Token]): Trampoline[List[Content]] = {
      ts match {
        case Nil => Return[Function0, List[Content]](Nil)
        case _::_ => {
          val (es, _, ts1) = NSInfo.nsInfo(Nil).nodes(Nil, ts)
          Suspend(() => tparse(ts1)) map (es ::: _)
        }
      }
    }

    tparse(ts).run
  }

  /// tag
  private[xml] def tagT(s: LStr): Trampoline[List[Token]] =
    s match {
      case (p, '/')::cs => {
        val (n, ds) = qualName(dropSpace(cs))
        Suspend(() => XSource.tokensT(ds match {
          case (_, '>') :: es => es
          case _ => ds
        }) map (endToken(p, n) :: _))
      }
      case Nil =>
        Return[Function0, List[Token]](Nil)
      case (l, _) :: _ => {
        val (n, ds) = qualName(dropSpace(s))
        Suspend(() => attribsT(dropSpace(ds)) map {
          case (as, b, ts) =>
            startToken(l, n, as, b) :: ts
        })
      }
    }

  /// attribs
  private[xml] def attribsT(s: LStr): Trampoline[(List[Attr], Boolean, List[Token])] =
    s match {
      case (_, '>') :: ds =>
        XSource.tokensT(ds) map ((Nil, false, _))
      case (_, '/') :: ds =>
        Suspend(() => (XSource.tokensT(ds match {
          case (_, '>') :: es => es
          case _ => ds
        })) map ((Nil, true, _)))
      case (_, '?') :: (_, '>') :: ds =>
        Suspend(() => XSource.tokensT(ds) map ((Nil, true, _)))
      case Nil =>
        Return[Function0, (List[Attr], Boolean, List[Token])]((Nil, false, Nil))
      case _ => {
        val (a, cs1) = attrib(s)
        Suspend(() => attribsT(cs1) map {
          case (as, b, ts) => (a::as, b, ts)
        })
      }
    }

}

object Token extends Tokens
