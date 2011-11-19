package scalaz
package std

trait IterableInstances {

  implicit def IterableShow[CC[X] <: Iterable[X], A: Show]: Show[CC[A]] = new Show[CC[A]] {
    import syntax.show._
    def show(as: CC[A]) = {
      val i = as.iterator
      val k = new collection.mutable.ListBuffer[Char]
      k += '['
      while (i.hasNext) {
        val n = i.next
        k ++= n.show
        if (i.hasNext)
          k += ','
      }
      k += ']'
      k.toList
    }
  }
}

object iterable extends IterableInstances {
  object iterableSyntax extends scalaz.syntax.std.ToIterableV
}