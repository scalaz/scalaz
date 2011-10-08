package scalaz
package undo

case class History[S](current: S, undos: List[S] = Nil, redos: List[S] = Nil)

object History {
  // TODO base this on Show[S]
  implicit object HistoryIsShow extends Show[History[_]] {
    def show(a: History[_]) = a.toString.toList
  }
}
