package scalaz
package xml
package cursor

sealed trait History {
  val value: Vector[Op]

  def :+(o: Op): History =
    new History {
      val value = History.this.value :+ o
    }

  def +:(o: Op): History =
    new History {
      val value = o +: History.this.value
    }

  def ++(h: History): History =
    new History {
      val value = History.this.value ++ h.value
    }
}

trait Historys {
  def history: History =
    new History {
      val value = Vector.empty
    }

  def apply(o: Op): History =
    new History {
      val value = Vector(o)
    }

  import std.AllInstances._

  implicit val HistoryShow: Show[History] = new Show[History] {
    def show(h: History) =
      Show[List[Op]].show(h.value.toList).toList
  }

  implicit val HistoryEqual: Equal[History] =
    Equal.equalBy(_.value.toList)

}

object History extends Historys
