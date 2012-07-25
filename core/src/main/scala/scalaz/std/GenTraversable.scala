package scalaz
package std

import scala.annotation.tailrec
import scala.collection.GenTraversable

trait GenTraversableInstances {

  implicit def genTraversableSorter[CC[X] <: GenTraversable[X]]: Sorter[CC] = new Sorter[CC] {
    def merge[A:Order](fa: CC[A], y: A): CC[A] = {
      val (ps,ss) = fa.partition(implicitly[Order[A]].lessThan(_,y))
      val builder = fa.genericBuilder[A]
      builder ++= ps.seq
      builder  += y
      builder ++= ss.seq
      builder.result.asInstanceOf[CC[A]]
    }

    @tailrec
    def mergeAll[A:Order](fa: CC[A], ys: CC[A]): CC[A] =
      if (ys.isEmpty) fa else mergeAll(merge(fa,ys.head), ys.tail.asInstanceOf[CC[A]])

    override def sort[A:Order](fa: CC[A]): CC[A] =
      fa.aggregate(fa.genericBuilder[A].result.asInstanceOf[CC[A]])(merge, mergeAll)
  }

}

object genTraversable extends GenTraversableInstances
