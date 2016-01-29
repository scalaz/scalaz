package scalaz

/** A singly-linked list that is guaranteed to be non-empty. */
sealed trait NonEmptyList[+A] {
  val head: A
  val tail: List[A]

  import NonEmptyList._
  import Zipper._

  def <::[AA >: A](b: AA): NonEmptyList[AA] = nel(b, head :: tail)

  def <:::[AA >: A](bs: List[AA]): NonEmptyList[AA] = bs match {
    case Nil => this
    case b :: bs => nel(b, bs ::: list)
  }

  def :::>[AA >: A](bs: List[AA]): NonEmptyList[AA] = nel(head, tail ::: bs)

  /** Append one nonempty list to another. */
  def append[AA >: A](f2: NonEmptyList[AA]): NonEmptyList[AA] = list <::: f2

  def map[B](f: A => B): NonEmptyList[B] = nel(f(head), tail.map(f))

  /** @since 7.0.3 */
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  import collection.mutable.ListBuffer

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

  def traverse1[F[_], B](f: A => F[B])(implicit F: Apply[F]): F[NonEmptyList[B]] = {
    import std.list._
    tail match {
      case Nil => F.map(f(head))(nel(_, Nil))
      case b :: bs => F.apply2(f(head), OneAnd.oneAndTraverse[List].traverse1(OneAnd(b, bs))(f)) {
        case (h, t) => nel(h, t.head :: t.tail)
      }
    }
  }

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

  /** @since 7.0.2 */
  def init: List[A] = if(tail.isEmpty) Nil else (head :: tail.init)

  /** @since 7.0.2 */
  def last: A = if(tail.isEmpty) head else tail.last

  def tails: NonEmptyList[NonEmptyList[A]] = {
    @annotation.tailrec
    def tails0(as: NonEmptyList[A], accum: List[NonEmptyList[A]]): NonEmptyList[NonEmptyList[A]] =
      as.tail match {
        case Nil => nel(as, accum).reverse
        case h :: t => tails0(nel(h, t), as :: accum)
      }
    tails0(this, Nil)
  }

  def reverse: NonEmptyList[A] = (list.reverse: @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  /** @since 7.0.2 */
  def sortBy[B](f: A => B)(implicit o: Order[B]): NonEmptyList[A] = (list.sortBy(f)(o.toScalaOrdering): @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  /** @since 7.0.2 */
  def sortWith(lt: (A, A) => Boolean): NonEmptyList[A] = (list.sortWith(lt): @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  /** @since 7.0.2 */
  def sorted[B >: A](implicit o: Order[B]): NonEmptyList[A] = (list.sorted(o.toScalaOrdering): @unchecked) match {
    case x :: xs => nel(x, xs)
  }

  def size: Int = 1 + tail.size

  def zip[B](b: => NonEmptyList[B]): NonEmptyList[(A, B)] = {
    val _b = b
    nel((head, _b.head), tail zip _b.tail)
  }

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

  def unapplySeq[A](v: NonEmptyList[A]): Option[(A, List[A])] =
    Some((v.head, v.tail))
}

trait NonEmptyListInstances0 {
  implicit def nonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] = Equal.equalBy[NonEmptyList[A], List[A]](_.list)(std.list.listEqual[A])
}

trait NonEmptyListInstances extends NonEmptyListInstances0 {
  implicit val nonEmptyList =
    new Traverse1[NonEmptyList] with Monad[NonEmptyList] with Plus[NonEmptyList] with Comonad[NonEmptyList] with Cobind.FromCojoin[NonEmptyList] with Each[NonEmptyList] with Zip[NonEmptyList] with Unzip[NonEmptyList] with Length[NonEmptyList] {
      def traverse1Impl[G[_] : Apply, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
        fa traverse1 f

      override def foldRight1[A](fa: NonEmptyList[A])(f: (A, => A) => A): A = {
        val reversed = fa.reverse
        reversed.tail.foldLeft(reversed.head)((x, y) => f(y, x))
      }

      override def foldLeft1[A](fa: NonEmptyList[A])(f: (A, A) => A): A = fa.tail match {
        case Nil => fa.head
        case h :: t => foldLeft1(NonEmptyList.nel(f(fa.head, h), t))(f)
      }

      override def foldMap1[A, B](fa: NonEmptyList[A])(f: A => B)(implicit F: Semigroup[B]): B = {
        fa.tail.foldLeft(f(fa.head))((x, y) => F.append(x, f(y)))
      }

      // would otherwise use traverse1Impl
      override def foldLeft[A, B](fa: NonEmptyList[A], z: B)(f: (B, A) => B): B =
        fa.tail.foldLeft(f(z, fa.head))(f)

      def bind[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] = fa flatMap f

      def point[A](a: => A): NonEmptyList[A] = NonEmptyList(a)

      def plus[A](a: NonEmptyList[A], b: => NonEmptyList[A]): NonEmptyList[A] = a.list <::: b

      def copoint[A](p: NonEmptyList[A]): A = p.head

      def cojoin[A](a: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] = a.tails

      def each[A](fa: NonEmptyList[A])(f: A => Unit) = fa.list foreach f

      def zip[A, B](a: => NonEmptyList[A], b: => NonEmptyList[B]) = a zip b

      def unzip[A, B](a: NonEmptyList[(A, B)]) = a.unzip

      def length[A](a: NonEmptyList[A]): Int = a.size

      override def all[A](fa: NonEmptyList[A])(f: A => Boolean) =
        f(fa.head) && fa.tail.forall(f)

      override def any[A](fa: NonEmptyList[A])(f: A => Boolean) =
        f(fa.head) || fa.tail.exists(f)
    }

  implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = new Semigroup[NonEmptyList[A]] {
    def append(f1: NonEmptyList[A], f2: => NonEmptyList[A]) = f1 append f2
  }

  implicit def nonEmptyListShow[A: Show]: Show[NonEmptyList[A]] = new Show[NonEmptyList[A]] {
    import std.list._
    override def show(fa: NonEmptyList[A]) = Show[List[A]].show(fa.list)
  }

  implicit def nonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] =
    Order.orderBy[NonEmptyList[A], List[A]](_.list)(std.list.listOrder[A])
}

trait NonEmptyListFunctions {
  def nel[A](h: A, t: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    val head = h
    val tail = t.toList
  }

  def nels[A](h: A, t: A*): NonEmptyList[A] =
    nel(h, t.toList)
}
