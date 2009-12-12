package scalaz

sealed trait NonEmptyList[+A] {
  val head: A
  val tail: List[A]

  import Scalaz._

  def <::[B >: A](b: B) = nel(b, head :: tail)

  import collection.mutable.ListBuffer

  def <:::[B >: A](bs: List[B]) = {
    val b = new ListBuffer[B]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def :::>[B >: A](bs: List[B]): NonEmptyList[B] = nel(head, tail ::: bs)

  def map[B](f: A => B) = nel(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B])  = {
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

  lazy val list = head :: tail

  lazy val stream = head #:: tail.toStream

  def tails : NonEmptyList[NonEmptyList[A]] = nel(this, tail match {
    case Nil => Nil
    case _ :: _ => tail.toNel.get.tails.list    
  })

  override def toString = "NonEmpty" + (head :: tail)
}

trait NonEmptyLists {
  def nel[A](h: A, t: List[A]): NonEmptyList[A] = NonEmptyList(h, t: _*)
}

object NonEmptyList {
  def apply[A](h: A, t: A*): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }
}