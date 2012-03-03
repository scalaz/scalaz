package scalaz
package xml
package cursor

import Predicate._
import HCursor._

sealed trait Shift {
  import Shift._

  def apply(c: Cursor): HCursor

  def run: Content => HCursor =
    t => apply(+t)

  def |||(s: => Shift): Shift =
    shift(c => {
      val w = apply(c)
      w.cursor match {
        case None =>
          s(c) withHistory (h => History(ChoiceSwitch(w.history, h)))
        case Some(_) =>
          w withHistory (h => History(ChoiceSucceed(h)))
      }
    })

  def >=> (s: => Shift): Shift =
    shift(c => {
      val w = apply(c)
      w.cursor match {
        case None =>
          w withHistory (_ :+ FailedCompose)
        case Some(d) =>
          s(d) withHistory (w.history ++ _)
      }
    })

  def *(n: Int): Shift =
    if(n <= 0)
      point
    else
      this >=> (this * (n - 1))

  def ->-(c: Cursor): Option[Cursor] =
    apply(c).cursor

  def =>=(c: Cursor): History =
    apply(c).history

  def cursorOr(c: Cursor, d: => Cursor): Cursor =
    ->-(c) getOrElse d

  def loop: Shift =
    this >=> loop

}

trait Shifts {
  def shift(k: Cursor => HCursor): Shift =
    new Shift {
      def apply(c: Cursor) =
        k(c)
    }

  def shifts(k: Cursor => Cursor): Shift =
      shift(c => hcursorc(cursor = k(c)))

  def shiftSplit(history: Cursor => History, cursor: Cursor => Option[Cursor]): Shift =
    shift(c => hcursor(history(c), cursor(c)))

  def shiftSplits(history: Cursor => History, cursor: Cursor => Cursor): Shift =
    shift(c => hcursorc(history(c), cursor(c)))

  def point: Shift =
    shift(c => hcursorc(cursor = c))

  def shiftConcat(s: Shift*): Shift =
    s.foldLeft(point)(_ >=> _)

  def shiftChoice(s: Shift*): Shift =
    s.foldLeft(shift(_ => hcursor()))(_ ||| _)

  def left: Shift =
    shift(LeftOp +++ _.left)

  def right: Shift =
    shift(RightOp +++ _.right)

  def firstChild: Shift =
    shift(FirstChild +++ _.firstChild)

  def lastChild: Shift =
    shift(LastChild +++ _.lastChild)

  // The new position is:
  // * right if exists, or
  // * left if exists, or
  // * parent
  def remove: Shift =
    shift(c => Remove +++ c.remove.map(_._2))
  
  def removeLeft: Shift =
    shift(c => RemoveLeft +++ c.removeLeft.map(_._2))

  def removeRight: Shift =
    shift(c => RemoveRight +++ c.removeRight.map(_._2))

  def parent: Shift =
    shift(Parent +++ _.parent)

  def root: Shift =
    shift(c => Root +++ Some(c.root))

  def findLeft(p: CPredicate): Shift =
    shift(FindLeft(p) +++ _.findLeft(p.pred))

  def findRight(p: CPredicate): Shift =
    shift(FindRight(p) +++ _.findRight(p.pred))

  def findChild(p: CPredicate): Shift =
    shift(FindChild(p) +++ _.findChild(p.pred))

  def nextDepthFirst: Shift =
    shift(NextDepthFirst +++ _.nextDepthFirst)

  def findRec(p: CPredicate): Shift =
    shift(FindRec(p) +++ _.findRec(p.pred))

  def nthChild(n: => Int): Shift =
    shift(NthChild(() => n) +++ _.nthChild(n))

}

object Shift extends Shifts
