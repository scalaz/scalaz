package scalaz
package xml

/**
 * A common trait for XML data source, such as a string, a list of characters, a stream, iteratee, etc. This allows common XML-operations to run on various data source types.
 */
sealed trait XSource[S] {
  val uncons: S => Option[(Char, S)]

  import Token._
  import CData._
  import Content._

  /// linenumber
  def linenumber(s: S, n: Line): LStr = {
    @annotation.tailrec
    def linenumberacc(s: S, n: Line, acc: LStr): LStr = {
      uncons(s) match {
        case None => acc
        case Some(('\r', t)) => linenumberacc(uncons(t) match {
          case Some(('\n', u)) => u
          case _ => t
        }, n + 1, (n, '\n')::acc)
        case Some(('\n', t)) => linenumberacc(t, n + 1, (n, '\n')::acc)
        case Some((c, t)) => linenumberacc(t, n, (n, c) :: acc)
      }
    }
    linenumberacc(s, n, Nil).reverse
  }

  /// tokens
  def tokens(s: S): List[Token] =
    XSource.tokens(linenumber(s, 1))

  /// parseXML
  def parseXml(s: S): List[Content] =
    Token.parse(tokens(s))

  /// parseXMLDoc
  def parseXmlDoc(s: S): Option[Element] = {
    def strip(cs: List[Content]): Option[Element] =
      Content.elems(cs) match {
        case Nil => None
        case e::es =>
          if(e.name.name startsWith "?xml")
            strip(es map (elem(_)))
          else
            Some(e)
      }

    strip(parseXml(s))
  }

}

trait XSources {
  def xsource[S](f: S => Option[(Char, S)]): XSource[S] =
    new XSource[S] {
      val uncons = f
    }

  import QName._
  import Token._
  import CDataKind._
  import CData._
  import Txt._

  /** A list of characters as an XML data source. */
  implicit val StrXSource: XSource[Str] =
    xsource {
      case Nil => None
      case h::t => Some(h, t)
    }

  // CAUTION: Uses unsafe code
  // DO NOT CHANGE TO val
  /** A string as an XML data source. */
  implicit def StringXSource: XSource[String] = {
    var i = 0
    xsource(s =>
      if(i == s.length)
        None
      else {
        val c = s(i)
        i = i + 1
        Some((c, s))
      })
  }

  /// tokens'
  def tokens(s: LStr): List[Token] =
    tokensT(s).run

  /// cref_to_char
  def crefToChar(s: Str): Option[Char] =
    s match {
      case '#'::ds => {
        val (w, rad) =
          ds match {
            case 'x'::es => (es, 16)
            case _ => (ds, 10)
          }
        try {
          val n = Integer.parseInt(w.mkString, rad)
          if(Char.MinValue.toInt <= n && n <= Char.MaxValue.toInt)
            Some(n.toChar)
          else
            None
        } catch {
          case _: NumberFormatException => None
        }
      }

      case List('l', 't') => Some('<')
      case List('g', 't') => Some('>')
      case List('a', 'm', 'p') => Some('&')
      case List('a', 'p', 'o', 's') => Some('\'')
      case List('q', 'u', 'o', 't') => Some('"')
      case _ => None
    }

  //// trampolined
  import Free._

  private[xml] def tokensT(s: LStr): Trampoline[List[Token]] =
    s match {
      case (_, '<') :: (c@(_, '!')) :: cs =>
        specialT(c, cs)
      case (_, '<') :: cs =>
        tagT(dropSpace(cs))
      case (l, _) :: _ => {
        val (as, bs) = breakn((_: Char) == '<', s)
        tokensT(bs) map (decodeText(as).map(_.fold(
            txt = x => textToken(cdata(
              verbatim = cdataText
            , data = x
            , line = Some(l)
            ))
          , cref = x => crefToChar(x) match {
              case Some(c) => textToken(cdata(
                verbatim = cdataText
              , data = List(c)
              , line = Some(l)
              ))
              case None => crefToken(x)
            }
          )) ::: _)
      }
      case Nil =>
        Return(Nil)
    }
  
  /// special
  private[xml] def specialT(c: LChar, s: LStr): Trampoline[List[Token]] =
    s match {
      case (_, '-') :: (_, '-') :: cs => {
        @annotation.tailrec
        def skip(z: LStr, acc: Str): Trampoline[List[Token]] =
          z match {
            case (_, '-') :: (_, '-') :: (_, '>') :: ds =>
              tokensT(ds) map (commentToken(acc.reverse) :: _)
            case (_, d) :: ds =>
              skip(ds, d :: acc)
            case Nil =>
              Return(Nil)
          }

        skip(cs, Nil)
      }
      case (_, '[') :: (_, 'C') :: (_, 'D') :: (_, 'A') :: (_, 'T') :: (_, 'A') :: (_, '[') :: cs => {
        def cdata(s: LStr): Trampoline[(Str, LStr)] =
          s match {
            case (_, ']') :: (_, ']') :: (_, '>') :: ds =>
              Return((Nil, ds))
            case (_, d) :: ds =>
              cdata(ds) map {
                case (xs, ys) => (d::xs, ys)
              }
            case Nil =>
              Return((Nil, Nil))
          }

        for {
          xsts <- cdata(cs)
          tts <- tokensT(xsts._2)
        } yield textToken(CData.cdata(
                  verbatim = cdataVerbatim
                , data = xsts._1
                , line = Some(c._1)
                )) :: tts
      }
      case _ => {
        @annotation.tailrec
        def munch(acc: Str, nesting: Int, z: LStr): Trampoline[(Str, LStr)] =
          z match {
            case (_, '>') :: ds =>
              if(nesting == 0)
                Return(('>'::acc, ds))
              else
                munch('>'::acc, nesting-1, ds)
            case (_, '<') :: ds =>
              munch('<'::acc, nesting+1, ds)
            case (_, x) :: ds =>
              munch(x::acc, nesting, ds)
            case Nil =>
              Return((acc, Nil))
          }


        for {
          xsts <- munch(Nil, 0, s)
          tts <- tokensT(xsts._2)
        } yield textToken(CData.cdata(
                  verbatim = cdataRaw
                , data = '<'::'!'::xsts._1.reverse
                , line = Some(c._1)
                )) :: tts
      }
    }

}

object XSource extends XSources
