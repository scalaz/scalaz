package scalaz.test

import scala._, scala.Predef._

import java.lang.{ Integer, StringBuilder }

import scalaz.data.{ IList, Maybe2 }

private[test] object util {
  def fastConcat(strs: IList[String]): String =
    fastConcatDelim(strs, "")

  def fastConcatDelim(strs: IList[String], delim: String): String = {
    var totalLength: Integer = Int.box(0)
    var numStrs: Integer     = Int.box(0)
    def go(cursor: IList[String]): Unit = IList.uncons(cursor) match {
      case Maybe2.Just2(s, ss) =>
        totalLength = Int.box(totalLength.intValue() + s.length + delim.length)
        numStrs = Int.box(numStrs.intValue() + 1)
        go(ss)
      case Maybe2.Empty2() =>
    }
    go(strs)
    val sb = new StringBuilder(totalLength + (numStrs * delim.length))
    def printOutDelimited(cursor: IList[String]): Unit = IList.uncons(cursor) match {
      case Maybe2.Just2(s, ss) =>
        sb.append(delim)
        sb.append(s)
        printOutDelimited(ss)
      case Maybe2.Empty2() =>
    }
    IList.uncons(strs) match {
      case Maybe2.Just2(s, ss) => sb.append(s); printOutDelimited(ss)
    }

    sb.toString
  }
}
