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

  implicit def IterableOrder[A: Order]: Order[Iterable[A]] = new Order[Iterable[A]] {
    def order(a1: Iterable[A], a2: Iterable[A]) = {
      import scalaz.Ordering._
      import syntax.order._
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = true
      var r: Ordering = EQ

      while (i1.hasNext && i2.hasNext && b) {
        val a1 = i1.next
        val a2 = i2.next

        val o = a1 ?|? a2
        if (o != EQ) {
          r = o
          b = false
        }
      }
      if (i1.hasNext)
        if (i2.hasNext)
          r
        else
          GT
      else
        LT
    }
  }
}

object iterable extends IterableInstances {
  object iterableSyntax extends scalaz.syntax.std.ToIterableV
}