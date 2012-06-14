package scalaz
package xml

import QName._

sealed trait CData {
  import CData._

  /// cdVerbatim
  val verbatim: CDataKind
  /// cdData
  val data: Str
  /// cdLine
  val line: Option[Line]

  def lineOr(l: => Line): Line =
    line getOrElse l

  def hasLine: Boolean =
    line.isDefined

  def setVerbatim(k: CDataKind): CData =
    cdata(k, data, line)

  def setData(d: Str): CData =
    cdata(verbatim, d, line)

  def setLine(l: Option[Line]): CData =
    cdata(verbatim, data, l)

}

trait CDatas {
  type Line =
  Long

  /// CData
  def cdata(verbatim: CDataKind, data: Str, line: Option[Line] = None): CData = {
    val v = verbatim
    val d = data
    val l = line
    new CData {
      val verbatim = v
      val data = d
      val line = l
    }
  }

  /// blank_cdata
  def blankCdata: CData =
    cdata(CDataKind.cdataText, Nil)

  import std.AllInstances._

  implicit val CDataEqual: Equal[CData] =
    Equal.equalBy[CData, (CDataKind, Str, Option[Line])](c => (c.verbatim, c.data, c.line))

  implicit val CDataShow: Show[CData] = new Show[CData] {
    def show(c: CData) =
      ("CData{verbatim=" + Show[CDataKind].shows(c.verbatim) + ",data=" + c.data.mkString + (c.line match {
        case None => ""
        case Some(l) => ",line=" + l
      }) + "}").toList
  }

}

object CData extends CDatas {

  import Lens._
  import StoreT._

  val verbatimCDataL: CData @> CDataKind =
    lens(x => store(x.verbatim)(b => cdata(b, x.data, x.line)))

  val dataCDataL: CData @> Str =
    lens(x => store(x.data)(b => cdata(x.verbatim, b, x.line)))

  val lineCDataL: CData @> Option[Line] =
    lens(x => store(x.line)(b => cdata(x.verbatim, x.data, b)))

}
