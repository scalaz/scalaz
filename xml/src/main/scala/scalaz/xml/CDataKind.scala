package scalaz
package xml

sealed trait CDataKind {
  def fold[X](
    text: => X
  , verbatim: => X
  , raw: => X
  ): X =
    this match {
      case CDataText => text
      case CDataVerbatim => verbatim
      case CDataRaw => raw
    }

  def isText: Boolean =
    fold(true, false, false)

  def isVerbatim: Boolean =
    fold(false, true, false)

  def isRaw: Boolean =
    fold(false, false, true)
}
private case object CDataText extends CDataKind
private case object CDataVerbatim extends CDataKind
private case object CDataRaw extends CDataKind

trait CDataKinds {
  /// CDataText
  val cdataText: CDataKind =
    CDataText

  /// CDataVerbatim
  val cdataVerbatim: CDataKind =
    CDataVerbatim

  /// CDataRaw
  val cdataRaw: CDataKind =
    CDataRaw

  implicit def CDataKindEqual: Equal[CDataKind] =
    Equal.equalA

  implicit def CDataKindShow: Show[CDataKind] =
    Show.showFromToString

}

object CDataKind extends CDataKinds
