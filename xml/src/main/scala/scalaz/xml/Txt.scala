package scalaz
package xml

import QName._

sealed trait Txt {
  val isTxt: Boolean
  val str: Str

  def isCref =
    !isTxt

  def fold[X](
    txt: Str => X
  , cref: Str => X
  ): X =
    (if(isTxt) txt else cref)(str)
}

trait Txts {
  def txtBit(s: Str): Txt =
    new Txt {
      val isTxt = true
      val str = s
    }

  def crefBit(s: Str): Txt =
    new Txt {
      val isTxt = false
      val str = s
    }

  /// decode_text
  def decodeText(s: Str): List[Txt] =
    s match {
      case '&'::cs =>
        cs span (_ != ';') match {
          case (as, _::bs) =>
            crefBit(as) :: decodeText(bs)
          case _ =>
            List(txtBit(s))
        }
      case Nil => Nil
      case _ => {
        val (as, bs) = s span (_ != '&')
        txtBit(as) :: decodeText(bs)
      }
    }

  /// decode_attr
  def decodeAttr(s: Str): Str =
    decodeText(s) flatMap (_.fold(
      txt = x => x
    , cref = x => XSource.crefToChar(x) match {
        case Some(c) => List(c)
        case None => '&'::x ::: List(';')
      }
    ))

}

object Txt extends Txts {

  import Lens._
  import CostateT._

  val is_txtTxtL: Txt @> Boolean =
    lens(x => costate(b => if(b) txtBit(x.str) else crefBit(x.str), x.isTxt))

  val strTxtL: Txt @> Str =
    lens(x => costate(b => if(x.isTxt) txtBit(b) else crefBit(b), x.str))

}
