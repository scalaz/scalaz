package scalaz
package xml
package cursor

sealed trait HCursor {
  val history: History
  val cursor: Option[Cursor]

  import HCursor._

  def unary_- : Option[Content] =
    cursor map (-_)

  def setHistory(h: History): HCursor =
    hcursor(h, cursor)

  def withHistory(h: History => History): HCursor =
    hcursor(h(history), cursor)

  def setNoHistory: HCursor =
    setHistory(History.history)

  def setCursor(c: Option[Cursor]): HCursor =
    hcursor(history, c)

  def withCursor(c: Option[Cursor] => Option[Cursor]): HCursor =
    hcursor(history, c(cursor))

  def fwithCursor(c: Cursor => Cursor): HCursor =
    withCursor(_ map c)

  def setNoCursor: HCursor =
    setCursor(None)

  def :+(o: Op): HCursor =
    withHistory(_ :+ o)

  def +:(o: Op): HCursor =
    withHistory(o +: _)

  def ++(h: History): HCursor =
    withHistory(_ ++ h)

  def |||(h: HCursor): HCursor =
    hcursor(history ++ h.history, cursor orElse h.cursor)
}

trait HCursors {
  def hcursor(history: History = History.history, cursor: Option[Cursor] = None): HCursor = {
    val h = history
    val c = cursor
    new HCursor {
      val history = h
      val cursor = c
    }
  }

  def hcursorc(history: History = History.history, cursor: Cursor): HCursor =
    hcursor(history, Some(cursor))

  import std.AllInstances._

  implicit val HCursorShow: Show[HCursor] = new Show[HCursor] {
    def show(c: HCursor) =
      ("HCursor{history=" + implicitly[Show[History]].shows(c.history) + (c.cursor match {
        case None => ""
        case Some(q) => ",cursor=" + implicitly[Show[Cursor]].shows(q)
      }) + "}").toList
  }

  implicit val HCursorEqual: Equal[HCursor] =
    Equal.equalBy(c => (c.history, c.cursor))

}

object HCursor extends HCursors {

  import Lens._
  import CostateT._

  val historyHCursorL: HCursor @> History =
    lens(x => costate(b => hcursor(b, x.cursor), x.history))

  val cursorHCursorL: HCursor @> Option[Cursor] =
    lens(x => costate(b => hcursor(x.history, b), x.cursor))

}
