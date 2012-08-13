package scalaz

/** A singly-linked list that is guaranteed to be non-empty. */
sealed trait NonEmptyList[+A] {
  val head: A
  val tail: List[A]

  import NonEmptyList._
  import Zipper._

  def <::[AA >: A](b: AA): NonEmptyList[AA] = nel(b, head :: tail)

  import collection.mutable.ListBuffer

  def <:::[AA >: A](bs: List[AA]): NonEmptyList[AA] = {
    val b = new ListBuffer[AA]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def :::>[AA >: A](bs: List[AA]): NonEmptyList[AA] = nel(head, tail ::: bs)

  /** Append one nonempty list to another. */
  def append[AA >: A](f2: NonEmptyList[AA]): NonEmptyList[AA] = list <::: f2

  def map[B](f: A => B): NonEmptyList[B] = nel(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = {
    val b = new ListBuffer[B]
    val p = f(head)
    b += p.head
    b ++= p.tail
    tail.foreach {
      a =>
        val p = f(a)
        b += p.head
        b ++= p.tail
    }
    val bb = b.toList
    nel(bb.head, bb.tail)
  }

  def traverse[G[_] : Applicative, B](f: A => G[B]): G[NonEmptyList[B]] = {
    import std.list.listInstance

    Applicative[G].map(Traverse[List].traverse(list)(f))(bs => NonEmptyList.nel(bs.head, bs.tail))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = list.foldRight(z)((a, b) => f(a, b))

  def list: List[A] = head :: tail

  def stream: Stream[A] = head #:: tail.toStream

  def toZipper: Zipper[A] = zipper(Stream.Empty, head, tail.toStream)

  def zipperEnd: Zipper[A] = {
    import Stream._
    tail.reverse match {
      case Nil     => zipper(empty, head, empty)
      case t :: ts => zipper(ts.toStream :+ head, t, empty)
    }
  }

  def tails: NonEmptyList[NonEmptyList[A]] = nel(this, tail match {
    case Nil    => Nil
    case h :: t => nel(h, t).tails.list
  })

  def reverse: NonEmptyList[A] = (list.reverse: @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  def size: Int = 1 + tail.size

  def zip[B](b: => NonEmptyList[B]): NonEmptyList[(A, B)] =
    nel((head, b.head), tail zip b.tail)

  def unzip[X, Y](implicit ev: A <:< (X, Y)): (NonEmptyList[X], NonEmptyList[Y]) = {
    val (a, b) = head: (X, Y)
    val (aa, bb) = tail.unzip: (List[X], List[Y])
    (nel(a, aa), nel(b, bb))
  }

  override def toString: String = "NonEmpty" + (head :: tail)

  override def equals(any: Any): Boolean =
    any match {
      case that: NonEmptyList[_] => this.list == that.list
      case _                     => false
    }

  override def hashCode: Int =
    list.hashCode
}

object NonEmptyList extends NonEmptyListFunctions with NonEmptyListInstances {
  def apply[A](h: A, t: A*): NonEmptyList[A] =
    nels(h, t: _*)
}

trait NonEmptyListInstances {
  implicit val nonEmptyList =
    new Traverse[NonEmptyList] with Monad[NonEmptyList] with Plus[NonEmptyList] with Comonad[NonEmptyList] with Cobind.FromCojoin[NonEmptyList] with Each[NonEmptyList] with Zip[NonEmptyList] with Unzip[NonEmptyList] {
      def traverseImpl[G[_] : Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
        fa traverse f

      override def foldRight[A, B](fa: NonEmptyList[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)

      def bind[A, B](fa: NonEmptyList[A])(f: (A) => NonEmptyList[B]): NonEmptyList[B] = fa flatMap f

      def point[A](a: => A): NonEmptyList[A] = NonEmptyList(a)

      def plus[A](a: NonEmptyList[A], b: => NonEmptyList[A]): NonEmptyList[A] = a.list <::: b

      def copoint[A](p: NonEmptyList[A]): A = p.head

      def cojoin[A](a: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] = a.tails

      def each[A](fa: NonEmptyList[A])(f: A => Unit) = fa.list foreach f

      def zip[A, B](a: => NonEmptyList[A], b: => NonEmptyList[B]) = a zip b

      def unzip[A, B](a: NonEmptyList[(A, B)]) = a.unzip
    }

  implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = new Semigroup[NonEmptyList[A]] {
    def append(f1: NonEmptyList[A], f2: => NonEmptyList[A]) = f1 append f2
  }

  implicit def nonEmptyListShow[A: Show]: Show[NonEmptyList[A]] = new Show[NonEmptyList[A]] {
    import std.list._
    override def show(fa: NonEmptyList[A]) = Show[List[A]].show(fa.list)
  }

  implicit def nonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] = Equal.equalBy[NonEmptyList[A], List[A]](_.list)(std.list.listEqual[A])
}

trait NonEmptyListFunctions {
  def nel[A](h: A, t: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }

  def nels[A](h: A, t: A*): NonEmptyList[A] =
    nel(h, t.toList)
}
