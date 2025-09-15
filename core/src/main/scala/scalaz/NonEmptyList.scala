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
    tail.foldLeft(()){(_, a) =>
      f(a)
      ()
    }
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

  def toZipper: Zipper[A] = zipper(LazyList.empty, head, tail.toLazyList)

  def zipperEnd: Zipper[A] = {
    import LazyList._
    tail.reverse match {
      case INil() => zipper(empty, head, empty)
      case ICons(t, ts) => zipper(ts.toLazyList :+ head, t, empty)
    }
  }

  /** @since 7.0.2 */
  def init: IList[A] = tail.initMaybe.cata(il => head :: il, IList.empty[A])

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
      case that: NonEmptyList[?] => this.list == that.list
      case _                     => false
    }

  override def hashCode: Int =
    list.hashCode
}

object NonEmptyList extends NonEmptyListInstances {
  // optimised versions of apply(A*)
  @inline final def apply[A](a: A): NonEmptyList[A] = nel(a, IList.empty)
  @inline final def apply[A](a: A, b: A): NonEmptyList[A] = nel(a, IList(b))
  @inline final def apply[A](a: A, b: A, c: A): NonEmptyList[A] = nel(a, IList(b, c))
  @inline final def apply[A](a: A, b: A, c: A, d: A): NonEmptyList[A] = nel(a, IList(b, c, d))
  @inline final def apply[A](a: A, b: A, c: A, d: A, e: A): NonEmptyList[A] = nel(a, IList(b, c, d, e))
  @inline final def apply[A](a: A, b: A, c: A, d: A, e: A, f: A): NonEmptyList[A] = nel(a, IList(b, c, d, e, f))

  @inline final def apply[A](a: A, b: A, c: A, d: A, e: A, f: A, as: A*): NonEmptyList[A] = a <:: b <:: c <:: d <:: e <:: fromSeq(f, as)

  def fromSeq[A](h: A, t: Seq[A]): NonEmptyList[A] =
    nel(h, IList.fromSeq(t))

  def unapply[A](v: NonEmptyList[A]): Some[(A, IList[A])] =
    Some((v.head, v.tail))

  def nel[A](h: A, t: IList[A]): NonEmptyList[A] =
    new NonEmptyList(h, t)

  def lift[A, B](f: NonEmptyList[A] => B): IList[A] => Option[B] = {
    case INil() => None
    case ICons(h, t) => Some(f(NonEmptyList.nel(h, t)))
  }
}

sealed abstract class NonEmptyListInstances0 {
  implicit def nonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] =
    Equal.equalBy[NonEmptyList[A], IList[A]](_.list)(using IList.equal[A])
}

sealed abstract class NonEmptyListInstances extends NonEmptyListInstances0 {
  implicit val nonEmptyListIsCovariant: IsCovariant[NonEmptyList] =
    IsCovariant.force[NonEmptyList]

  implicit val nonEmptyList: Traverse1[NonEmptyList] & Monad[NonEmptyList] & Alt[NonEmptyList] & BindRec[NonEmptyList] & Plus[NonEmptyList] & Comonad[NonEmptyList] & Zip[NonEmptyList] & Unzip[NonEmptyList] & Align[NonEmptyList] =
    new Traverse1[NonEmptyList] with Monad[NonEmptyList] with Alt[NonEmptyList] with BindRec[NonEmptyList] with Plus[NonEmptyList] with Comonad[NonEmptyList] with Zip[NonEmptyList] with Unzip[NonEmptyList] with Align[NonEmptyList] {
      override def findLeft[A](fa: NonEmptyList[A])(f: A => Boolean) =
        if(f(fa.head)) Some(fa.head) else fa.tail.find(f).toOption

      override def foldMap[A, B](fa: NonEmptyList[A])(f: A => B)(implicit M: Monoid[B]) =
        Foldable[IList].foldMap(fa.list)(f)(M)

      override def traverse1[F[_], A, B](fa: NonEmptyList[A])(f: A => F[B])(implicit F: Apply[F]) = {
        val revOpt: Maybe[F[NonEmptyList[B]]] =
          F.unfoldrOpt[IList[A], B, NonEmptyList[B]](fa.list){
            case ICons(a, as) => Maybe.just((f(a), as))
            case INil() => Maybe.empty
          }(Reducer.ReverseNonEmptyListReducer[B])

        val rev: F[NonEmptyList[B]] = revOpt getOrElse sys.error("Head cannot be empty")
        F.map(rev)(_.reverse)
      }

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

      override def psumMap1[A, B, G[_]](fa: NonEmptyList[A])(f: A => G[B])(implicit G: Plus[G]): G[B] =
        fa.tail match {
          case INil() => f(fa.head)
          case ICons(snd, rest) => G.plus(f(fa.head), psumMap1(NonEmptyList.nel(snd, rest))(f)(G))
        }

      // would otherwise use traverse1Impl
      override def foldLeft[A, B](fa: NonEmptyList[A], z: B)(f: (B, A) => B): B =
        fa.tail.foldLeft(f(z, fa.head))(f)

      def bind[A, B](fa: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] = fa flatMap f

      def point[A](a: => A): NonEmptyList[A] = NonEmptyList(a)

      def plus[A](a: NonEmptyList[A], b: => NonEmptyList[A]): NonEmptyList[A] = a.list <::: b

      def alt[A](a: => NonEmptyList[A], b: => NonEmptyList[A]): NonEmptyList[A] = plus(a, b)

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

      def tailrecM[A, B](a: A)(f: A => NonEmptyList[A \/ B]): NonEmptyList[B] =
        (BindRec[IList].tailrecM[A, B](a)(a => f(a).list): @unchecked) match {
          case ICons(h, t) => NonEmptyList.nel(h, t)
        }
    }

  implicit def nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] = new Semigroup[NonEmptyList[A]] {
    def append(f1: NonEmptyList[A], f2: => NonEmptyList[A]) = f1 append f2
  }

  implicit def nonEmptyListShow[A: Show]: Show[NonEmptyList[A]] =
    Contravariant[Show].contramap(IList.show[A])(_.list)

  implicit def nonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] =
    Order.orderBy[NonEmptyList[A], IList[A]](_.list)(using IList.order[A])
}
