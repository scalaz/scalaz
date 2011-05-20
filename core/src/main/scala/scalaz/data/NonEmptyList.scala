package scalaz
package data

sealed trait NonEmptyList[A] {
  val head: A
  val tail: List[A]

  import NonEmptyList._
  import Zipper._

  def <::(b: A): NonEmptyList[A] = nel(b, head :: tail)

  import collection.mutable.ListBuffer

  def <:::(bs: List[A]): NonEmptyList[A] = {
    val b = new ListBuffer[A]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def :::>(bs: List[A]): NonEmptyList[A] = nel(head, tail ::: bs)

  def map[B](f: A => B): NonEmptyList[B] = nel(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = {
    val b = new ListBuffer[B]
    val p = f(head)
    b += p.head
    b ++= p.tail
    tail.foreach(a => {
      val p = f(a)
      b += p.head
      b ++= p.tail
    })
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def list: List[A] = head :: tail

  def stream: Stream[A] = head #:: tail.toStream

  def toZipper: Zipper[A] = zipper(Stream.Empty, head, tail.toStream)

  def zipperEnd: Zipper[A] = {
    import Stream._
    tail reverse match {
      case Nil => zipper(empty, head, empty)
      case t :: ts => zipper(ts.toStream :+ head, t, empty)
    }
  }

  def tails: NonEmptyList[NonEmptyList[A]] = nel(this, tail match {
    case Nil => Nil
    case h :: t => nel(h, t).tails.list
  })

  def reverse: NonEmptyList[A] = (list.reverse: @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  override def toString: String = "NonEmpty" + (head :: tail)
}

object NonEmptyList extends NonEmptyLists {
  def apply[A](h: A, t: List[A]): NonEmptyList[A] =
    nel(h, t)
}

trait NonEmptyLists {
  def nel[A](h: A, t: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }

  def nels[A](h: A, t: A*): NonEmptyList[A] =
    nel(h, t.toList)

  implicit val NonEmptyListPointed: Pointed[NonEmptyList] = new Pointed[NonEmptyList] {
    def point[A](a: => A) = nel(a, Nil)
  }

  implicit def NonEmptyListCojoin: CoJoin[NonEmptyList] = new CoJoin[NonEmptyList] {
    def coJoin[A] = _.tails
  }

  implicit val NonEmptyListFoldl: Foldl[NonEmptyList] = new Foldl[NonEmptyList] {
    def foldl[A, B] = k => b => _.list.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit val NonEmptyListFoldr: Foldr[NonEmptyList] = new Foldr[NonEmptyList] {
    def foldr[A, B] = k => b => _.list.foldRight(b)((b, a) => k(b)(a))
  }

  implicit val NonEmptyListFoldable: Foldable[NonEmptyList] =
    Foldable.foldable[NonEmptyList]

  implicit def NonEmptyListShow[A: Show]: Show[NonEmptyList[A]] =
    implicitly[Show[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

  implicit def NonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] =
    implicitly[Equal[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

  implicit def NonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] =
    implicitly[Order[Iterable[A]]] contramap ((_: NonEmptyList[A]).list)

}
