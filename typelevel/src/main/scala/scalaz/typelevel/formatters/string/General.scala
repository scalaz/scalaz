package scalaz
package typelevel.formatters.string

import UnionTypes._
import typelevel.Format
import typelevel.SimpleUnionFormat

trait General {

  def bool(width: Int = 0, maxLength: Int = 0, left: Boolean = false) = Format((x: Any) =>
    (
      "%" +
      (if (left) "-" else "") +
      (if (width > 0) width else "") +
      (if (maxLength > 0) "."+maxLength else "") +
      "b"
    ) format x
  )

  def hex(width: Int = 0, maxLength: Int = 0, left: Boolean = false) = Format((x: Any) =>
    (
      "%" +
      (if (left) "-" else "") +
      (if (width > 0) width else "") +
      (if (maxLength > 0) "."+maxLength else "") +
      "h"
    ) format x
  )

  def str(width: Int = 0, maxLength: Int = 0, left: Boolean = false) = Format((x: Any) =>
    (
      "%" +
      (if (left) "-" else "") +
      (if (width > 0) width else "") +
      (if (maxLength > 0) "."+maxLength else "") +
      "s"
    ) format x
  )

  case class char(width: Int = 0, left: Boolean = false) extends SimpleUnionFormat[t[Char]#t[Byte]#t[Short], String](x =>
    (
      "%" +
      (if (left) "-" else "") +
      (if (width > 0) width else "") +
      "c"
    ) format x
  )

}

object General extends General

// vim: expandtab:ts=2:sw=2
