package scalaz

/** A singly-linked list that is guaranteed to be non-empty. */
final class NonEmptyList[A] private[scalaz](val head: A, val tail: IList[A]) {
  import NonEmptyList._
  import Zipper._
  import scalaz.Liskov._
  

  def <::(b: A): NonEmptyList[A] = nel(b, head :: tail)

  def <:::(bs: IList[A]): NonEmptyList[A] = bs match {
    case INil() => this
    case ICons(b, bs) => nel(b, bs ::: list)
  }

  def :::>(bs: IList[A]): NonEmptyList[A] = nel(head, tail ::: bs)

  /** Append one nonempty list to another. */
  def append(f2: NonEmptyList[A]): NonEmptyList[A] = list <::: f2

  def map[B](f: A => B): NonEmptyList[B] = nel(f(head), tail.map(f))

  /** @since 7.0.3 */
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.toList.foreach(f)
  }

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = {
    val rev = reverse
    rev.tail.foldLeft(f(rev.head))((nel, b) => f(b) append nel)
  }

  def distinct(implicit A: Order[A]): NonEmptyList[A] =
    (list.distinct: @unchecked) match {
      case ICons(x, xs) => nel(x, xs)
    }

  def traverse1[F[_], B](f: A => F[B])(implicit F: Apply[F]): F[NonEmptyList[B]] = {
    tail match {
      case INil() => F.map(f(head))(nel(_, IList.empty))
      case ICons(b, bs) => F.apply2(f(head), OneAnd.oneAndTraverse[IList].traverse1(OneAnd(b, bs))(f)) {
        case (h, t) => nel(h, t.head :: t.tail)
      }
    }
  }

  def list: IList[A] = head :: tail

  def stream: Stream[A] = head #:: tail.toStream

  def toZipper: Zipper[A] = zipper(Stream.Empty, head, tail.toStream)

  def zipperEnd: Zipper[A] = {
    import Stream._
    tail.reverse match {
      case INil()     => zipper(empty, head, empty)
      case ICons(t, ts) => zipper(ts.toStream :+ head, t, empty)
    }
  }

  /** @since 7.0.2 */
  def init: IList[A] = tail.initOption.fold[IList[A]](IList.empty)(il => head :: il)

  def inits: NonEmptyList[NonEmptyList[A]] =
    reverse.tails.map(_.reverse)

  /** @since 7.0.2 */
  def last: A = tail.lastOption.getOrElse(head)

  def tails: NonEmptyList[NonEmptyList[A]] = {
    @annotation.tailrec
    def tails0(as: NonEmptyList[A], accum: IList[NonEmptyList[A]]): NonEmptyList[NonEmptyList[A]] =
      as.tail match {
        case INil() => nel(as, accum).reverse
        case ICons(h, t) => tails0(nel(h, t), as :: accum)
      }
    tails0(this, IList.empty)
  }

  def reverse: NonEmptyList[A] = (list.reverse: @unchecked) match {
    case ICons(x, xs) => nel(x, xs)
  }

  /** @since 7.0.2 */
  
  def sortBy[B](f: A => B)(implicit o: Order[B]): NonEmptyList[A] = (list.sortBy(f): @unchecked) match {
    case ICons(x, xs) => nel(x, xs)
  }

  /** @since 7.0.2 */
  def sortWith(lt: (A, A) => Boolean): NonEmptyList[A] = 
    (list.toList.sortWith(lt): @unchecked) match {
      case x :: xs => nel(x, IList.fromList(xs))
    }

  /** @since 7.0.2 */
  def sorted(implicit o: Order[A]): NonEmptyList[A] = (list.sorted(o): @unchecked) match {
    case ICons(x, xs) => nel(x, xs)
  }

  def size: Int = 1 + tail.length

  def zip[B](b: => NonEmptyList[B]): NonEmptyList[(A, B)] = {
    val _b = b
    nel((head, _b.head), tail zip _b.tail)
  }

  def unzip[X, Y](implicit ev: A <~< (X, Y)): (NonEmptyList[X], NonEmptyList[Y]) = {
    val (a, b) = ev(head) 
    val (aa, bb) = tail.unzip: (IList[X], IList[Y])
    (nel(a, aa), nel(b, bb))
 }

  def zipWithIndex: NonEmptyList[(A, Int)] = {
    @annotation.tailrec
    def loop(as: IList[A], i: Int, acc: IList[(A, Int)]): IList[(A, Int)] =
      as match {
        case ICons(x, y) => loop(y, i + 1, (x, i) :: acc)
        case _ => acc.reverse
      }
    new NonEmptyList((head, 0), loop(tail, 1, IList.empty))
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

object NonEmptyList extends NonEmptyListInstances {
  def apply[A](h: A, t: A*): NonEmptyList[A] =
    nels(h, t: _*)

  def unapply[A](v: NonEmptyList[A]): Option[(A, IList[A])] =
    Some((v.head, v.tail))

  def nel[A](h: A, t: IList[A]): NonEmptyList[A] =
    new NonEmptyList(h, t)

  def nels[A](h: A, t: A*): NonEmptyList[A] =
    nel(h, IList(t: _*))

  def lift[A, B](f: NonEmptyList[A] => B): IList[A] => Option[B] = {
    case INil() ⇒ None
    case ICons(h, t) ⇒ Some(f(NonEmptyList.nel(h, t)))
  }
}

sealed abstract class NonEmptyListInstances0 {
  implicit def nonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] = Equal.equalBy[NonEmptyList[A], IList[A]](_.list)(IList.equal[A])
}

sealed abstract class NonEmptyListInstances extends NonEmptyListInstances0 {
  implicit val nonEmptyList: Traverse1[NonEmptyList] with Monad[NonEmptyList] with BindRec[NonEmptyList] with Plus[NonEmptyList] with Comonad[NonEmptyList] with Zip[NonEmptyList] with Unzip[NonEmptyList] with Align[NonEmptyList] =
    new Traverse1[NonEmptyList] with Monad[NonEmptyList] with BindRec[NonEmptyList] with Plus[NonEmptyList] with Comonad[NonEmptyList] with Zip[NonEmptyList] with Unzip[NonEmptyList] with Align[NonEmptyList] {
      override def findLeft[A](fa: NonEmptyList[A])(f: A => Boolean) =
        if(f(fa.head)) Some(fa.head) else fa.tail.find(f)

      def traverse1Impl[G[_] : Apply, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] =
        fa traverse1 f

      override def foldMapRight1[A, B](fa: NonEmptyList[A])(z: A => B)(f: (A, => B) => B): B = {
        val reversed = fa.reverse
        reversed.tail.foldLeft(z(reversed.head))((x, y) => f(y, x))
      }

      override def foldMapLeft1[A, B](fa: NonEmptyList[A])(z: A => B)(f: (B, A) => B): B =
        fa.tail.foldLeft(z(fa.head))(f)

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

      def cobind[A, B](fa: NonEmptyList[A])(f: NonEmptyList[A] => B): NonEmptyList[B] = map(cojoin(fa))(f)

      override def cojoin[A](a: NonEmptyList[A]): NonEmptyList[NonEmptyList[A]] = a.tails

      def zip[A, B](a: => NonEmptyList[A], b: => NonEmptyList[B]) = a zip b

      def unzip[A, B](a: NonEmptyList[(A, B)]) = a.unzip

      def alignWith[A, B, C](f: A \&/ B => C) = (a, b) => {
        NonEmptyList.nel(f(\&/.Both(a.head, b.head)), Align[IList].alignWith(f)(a.tail, b.tail))
      }

      override def length[A](a: NonEmptyList[A]): Int = a.size

      override def toNel[A](fa: NonEmptyList[A]) = fa

      override def toIList[A](fa: NonEmptyList[A]) = fa.list

      override def all[A](fa: NonEmptyList[A])(f: A => Boolean) =
        f(fa.head) && Foldable[IList].all(fa.tail)(f)

      override def any[A](fa: NonEmptyList[A])(f: A => Boolean) =
        f(fa.head) || Foldable[IList].any(fa.tail)(f)

      def tailrecM[A, B](f: A => NonEmptyList[A \/ B])(a: A): NonEmptyList[B] =
        (BindRec[IList].tailrecM[A, B](a => f(a).list)(a): @unchecked) match {
          case ICons(h, t) => NonEmptyList.nel(h, t)
        }
    }

  implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = new Semigroup[NonEmptyList[A]] {
    def append(f1: NonEmptyList[A], f2: => NonEmptyList[A]) = f1 append f2
  }

  implicit def nonEmptyListShow[A: Show]: Show[NonEmptyList[A]] =
    Contravariant[Show].contramap(IList.show[A])(_.list)

  implicit def nonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] =
    Order.orderBy[NonEmptyList[A], IList[A]](_.list)(IList.order[A])
}
