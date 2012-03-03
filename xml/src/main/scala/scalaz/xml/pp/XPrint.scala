package scalaz
package xml
package pp

import java.awt.event.KeyEvent


sealed trait XPrint[A] {
  val xprint: A => Config => List[Char]
  val xprints: A => Config => String

  def contramap[B](f: B => A): XPrint[B] =
    XPrint.xprint(b => XPrint.this.xprint(f(b)))
}

trait XPrints {
  def xprint[A](f: A => Config => List[Char]): XPrint[A] = new XPrint[A] {
    val xprint = f
    val xprints = (a: A) => (c: Config) => f(a)(c).mkString
  }

  def xprints[A](f: A => Config => String): XPrint[A] = new XPrint[A] {
    val xprint = (a: A) => (c: Config) => f(a)(c).toList
    val xprints = f
  }

  import QName._

  /// showQName
  def showQname(n: QName): List[Char] =
    n.prefix match {
      case None => n.name
      case Some(p) => p ::: ':' :: n.name
    }

  /// showAttr
  def showAttr(a: Attr): List[Char] =
    showQname(a.key) ::: '=' :: '"' :: esc_str(a.value) ::: List('"')

  /// showCDataS
  def showCdata(d: CData): List[Char] =
    d.verbatim.fold(
      text = esc_str(d.data)
    , verbatim = {
        def escape(c: Str): Str =
          c match {
            case ']' :: ']' :: '>' :: t => "]]]]><![CDATA[>".toList ::: escape(t)
            case h :: t => h :: escape(t)
            case Nil => Nil
          }
        "<![CDATA[".toList ::: escape(d.data) ::: "]]>".toList
      }
    , raw = d.data
    )

  implicit val CDataXPrint: XPrint[CData] =
    XPrint.xprint(t => pp_cdata(Nil, t))

  implicit val ElementXPrint: XPrint[Element] =
    XPrint.xprint(t => pp_element(Nil, t))

  implicit val ContentXPrint: XPrint[Content] =
    XPrint.xprint(t => pp_content(Nil, t))

  /// tagStart
  private def tagStart(n: QName, a: List[Attr]): List[Char] = {
    def intercalate[A](as: List[A], v: List[A]): List[A] = {
       val asr = as reverse
       @annotation.tailrec
       def intercalate0(accum: List[A], rest: List[A]): List[A] = rest match {
         case Nil => accum
         case x :: Nil => x :: accum
         case h :: t => intercalate0(asr ::: h :: accum, t)
       }
       intercalate0(Nil, v) reverse
     }

    '<' :: XPrint.showQname(n) ::: (
        if(a.isEmpty)
          Nil
        else
          ' ' :: intercalate(List(List(' ')), a map (XPrint.showAttr(_))).flatten
        )
  }

  /// tagEnd
  private def tag_end(n: QName): List[Char] =
    '<' :: '/' :: showQname(n) ::: List('>')

  /// escChar
  private def esc_char(c: Char) =
    c match {
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '&' => "&amp;"
      case '"' => "&quot;"
      case '\'' => "&#39;"
      case c => {
        def printable: Boolean = {
          val b = Character.UnicodeBlock.of(c)
          List(
            Character.isISOControl(c)
          , c == KeyEvent.CHAR_UNDEFINED
          , b == null
          , b == Character.UnicodeBlock.SPECIALS
          ) forall (!_)
        }

        if(c.toInt <= 0x7f || printable || c == '\n')
          c.toString
        else
          "&#" + c.toInt + ";"
      }
    }

  /// escStr
  private def esc_str(s: Str): List[Char] =
    s flatMap (esc_char(_))

  /// ppCDataS
  private def pp_cdata(i: Str, d: CData): Config => Str =
    c => i ::: (
        if(!d.verbatim.isText || c.isNotPrettify)
          showCdata(d)
        else
          showCdata(d).reverse.foldLeft(Nil: Str)((s, c) =>
            if(c == '\n')
              '\n' :: i ::: s
            else
              c :: s)
        )

  /// ppCDataS
  private def pp_element(i: Str, e: Element): Config => Str =
    c => {
      i ::: tagStart(e.name, e.attribs) ::: (e.content match {
        case Nil if e.name.name startsWith List('?') =>
          "?>".toList
        case Nil if c short_empty_tag e.name =>
          "/>".toList
        case t => {
          def j: List[Char] = {
            val (nl, sp) =
              c.prettify match {
                case None => (Nil, Nil)
                case Some(s) => ("\n".toList, s)
              }
            
            '>' :: nl ::: (t.reverse.foldLeft((i, i ::: tag_end(e.name))) {
              case ((ii, r), h) => (ii, pp_content(sp ::: ii, h)(c) ::: nl ::: r)
            })._2
          }
          t match {
            case Nil =>
              j
            case w::Nil =>
              w.text match {
                case None => j
                case Some(u) =>
                  '>' :: implicitly[XPrint[CData]].xprint(u)(c) ::: tag_end(e.name)
              }
            case _ =>
              j
          }
        }
      })
    }
  
  /// ppCDataS
  private def pp_content(i: Str, t: Content): Config => Str =
    c => t.fold(
        elem = e =>
            pp_element(i,
              if(c.isPrettify)
                e withContent (_ filter(_.text forall (_.data exists (!_.isWhitespace))))
              else
                e)(c)
      , text = d =>
           pp_cdata(i, d)(c)
      , cref = r =>
          '&' :: r ::: List(';')
      , comment = c =>
            i ::: '<' :: '!' :: '-' :: '-' :: c ::: List('-', '-', '>')
      )

}

object XPrint extends XPrints
