package scalaz

sealed trait NonEmptyList[+A] {
  val head: A
  val tail: List[A]

  import NonEmptyList.nel
  import ListW._

  def <::[B >: A](b: B) = nel(b, head :: tail)

  import scala.collection.mutable.ListBuffer

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
  
  val list = head :: tail

  val stream = Stream.cons(head, tail.projection)

  def tails : NonEmptyList[NonEmptyList[A]] = tail match {
    case Nil => nel(this)
    case h :: t => nel(this, tail.nel.get.tails.list)
  }

  override def toString = "NonEmpty" + (head :: tail)
}

object NonEmptyList {
  def nel[A](h: A, t: List[A]) = new NonEmptyList[A] {
    val head = h
    val tail = t
  }

  def nel[A](a: A): NonEmptyList[A] = nel(a, Nil)

  def nels[A](a: A, as: A*): NonEmptyList[A] = nel(a, as.toList)  
}
