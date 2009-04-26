package scalaz

import collection.immutable.IntMap

// todo broken

sealed trait BKTree[+A] {
  val value: A
  val children: Map[Int, BKTree[A]]

  import S._

  def +[AA >: A](a: AA)(implicit m: MetricSpace[AA]): BKTree[AA] = {
    val d = (value: AA) <===> a

    val j = children + ((d, children get d match {
      case None => a.bktree
      case Some(cs) => cs + a
    }))

    BKTree.bktree(a, j)
  }

  import Stream.cons

  def select(keys: List[Int]): List[BKTree[A]] = keys.foldRight(Nil: List[BKTree[A]])((k, x) => children get k match {
    case None => x
    case Some(a) => a :: x
  })

  def query[AA >: A](a: AA, n: Int)(implicit m: MetricSpace[AA]): List[(Int, AA)] = {
    val d = (value: AA) <===> a

    val c = select((d - n) to (d + n + 1) toList) flatMap(_.query(a, n))

    if(d <= n)
      (d, value) :: c
    else
      c
  }

  override def toString = value + " : " + children
}

object BKTree {
  def bktree[A](v: A): BKTree[A] = bktree(v, IntMap.empty)

  private def bktree[A](v: A, c: Map[Int, BKTree[A]]): BKTree[A] = new BKTree[A] {
    val value = v
    val children = c
  }
}
