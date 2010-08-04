package scalaz

sealed trait NonEmptyList[+A] {
  val head: A
  val tail: List[A]

  import Scalaz._

  def <::[B >: A](b: B): NonEmptyList[B] = nel(b, head :: tail)

  import collection.mutable.ListBuffer

  def <:::[B >: A](bs: List[B]): NonEmptyList[B] = {
    val b = new ListBuffer[B]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def :::>[B >: A](bs: List[B]): NonEmptyList[B] = nel(head, tail ::: bs)

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

  lazy val list: List[A] = head :: tail

  lazy val stream: Stream[A] = head #:: tail.toStream

  def toZipper: Zipper[A] = zipper(Stream.Empty, head, tail.toStream)

  def zipperEnd: Zipper[A] = {
    import Stream._
    tail reverse match {
      case Nil => zipper(empty, head, empty)
      case t :: ts => zipper(ts.toStream :+ head, t, empty)
    }
  }

  def tails: NonEmptyList[NonEmptyList[A]] = nel(this, tail.toNel match {
    case None => Nil
    case Some(t) => t.tails.list
  })

  def reverse: NonEmptyList[A] = (list.reverse: @unchecked) match {
    case x :: xs => nel(x, xs) 
  }

  override def toString: String = "NonEmpty" + (head :: tail)
}

trait NonEmptyLists {
  def nel[A](h: A, t: A*): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }

  def nel[A](h: A, t: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }

  @deprecated("use nel")
  def nel1[A](h: A, t: A*): NonEmptyList[A] = nel(h, t: _*)
}

object NonEmptyList extends NonEmptyLists {
  def apply[A](h: A, t: A*): NonEmptyList[A] = nel(h, t: _*)
}