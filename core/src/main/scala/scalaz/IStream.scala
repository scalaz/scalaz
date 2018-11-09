// Copyright: 2017 - 2018 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import scala.annotation.tailrec

/**
 * A linked list with by-name head and tail, allowing some control over memory
 * usage and evaluation of contents.
 *
 * This structure is a good choice when the contents are expensive to calculate
 * and may not all need to be evaluated, at the cost of an overhead when the
 * contents are calculated. Typeclass instances try to defer evaluation of
 * contents, unless it would require a full traversal of the spine, preferring
 * lazy semantics when an evaluation choice is to be made.
 */
sealed abstract class IStream[A] { self =>
  import IStream._

  /** Strict concatenation */
  final def !:(a: A): IStream[A]     = Cons(Value(a), Value(self))
  final def :!(other: A): IStream[A] = !!(Strict(other))
  final def !!(other: IStream[A]): IStream[A] =
    instances.foldRight(self, other)((a, as) => Strict.cons(a, as))

  /** prepend lazily (by-name parameters do not work here) */
  final def #:(a: Name[A]): IStream[A] = Cons(a, Value(self))

  // not stack safe. We could have an infinite stream so would we want to
  // sacrifice stack safety for non-terminating programs instead? That's what
  // foldLeft is for. Life is meaningless, etc, etc.
  final def foldRightByName[B](z: =>B)(f: (=>A, =>B) => B): B = self match {
    case _: Nil[_]        => z
    case Cons(head, tail) => f(head.value, tail.value.foldRightByName(z)(f))
  }

  // Stack safe, but can't exit early. You may never escape.
  final def foldLeftByName[B](z: B)(f: (=>B, =>A) => B): B = {
    @tailrec def loop(t: IStream[A], acc: B): B = t match {
      case _: Nil[_]        => acc
      case Cons(head, tail) => loop(tail.value, f(acc, head.value))
    }
    loop(self, z)
  }

  def reverse: IStream[A] = foldLeftByName(empty[A])((xs, x) => Lazy.cons(x, xs))

  def headMaybe: Maybe[A] = self match {
    case IStream.Nil() => Maybe.empty
    case IStream.Cons(head, _) => Maybe.just(head.value)
  }

}
object IStream {

  private final case class Nil[A]() extends IStream[A]
  private final case class Cons[A](
    head: Name[A],
    tail: Name[IStream[A]]
  ) extends IStream[A]
  // it is very tempting to add a third case that concatenates two streams...
  // but it would break the stack safety of foldLeft.

  def empty[A]: IStream[A]       = _empty.asInstanceOf[IStream[A]]
  private[this] final val _empty = Nil[Nothing]()

  object ByName {
    def apply[A](a: =>A): IStream[A] = Cons(Name(a), nil[A])
    def cons[A](head: =>A, tail: =>IStream[A]): IStream[A] = Cons(Name(head), Name(tail))
    def infinite[A](el: A): IStream[A] = cons(el, infinite(el))
  }
  object Lazy {
    def apply[A](a: =>A): IStream[A] = Cons(Need(a), nil[A])
    def cons[A](head: =>A, tail: =>IStream[A]): IStream[A] = Cons(Need(head), Need(tail))
    def infinite[A](el: A): IStream[A] = cons(el, infinite(el))
  }
  object Strict {
    def apply[A](a: A): IStream[A]                     = Cons(Value(a), nil[A])
    def cons[A](head: A, tail: IStream[A]): IStream[A] = head !: tail
  }

  def fromStream[A](sa: Stream[A]): IStream[A] = sa match {
    case Stream() => empty[A]
    case h #:: t  => Lazy.cons(h, fromStream(t))
  }

  def fromMaybe[A](ma: Maybe[A]): IStream[A] = ma match {
    case Maybe.Just(a) => Strict(a)
    case Maybe.Empty() => empty[A]
  }

  def fromFoldable[F[_]: Foldable, A](fa: F[A]): IStream[A] =
    Foldable[F].foldRight(fa, empty[A])((h, t) => Lazy.cons(h, t))

  // more efficient allocations
  private[this] final val __empty = Value(_empty)
  private[this] final def nil[A]  = __empty.asInstanceOf[Name[IStream[A]]]

  implicit val instances: MonadPlus[IStream] with IsEmpty[IStream] with Traverse[IStream] =
    new MonadPlus[IStream] with IsEmpty[IStream] with Traverse[IStream] {

      override def map[A, B](fa: IStream[A])(f: A => B): IStream[B] =
        fa.foldRightByName(empty[B])((h, t) => Lazy.cons(f(h), t))

      def point[A](a: =>A): IStream[A] = Lazy(a)
      def bind[A, B](fa: IStream[A])(f: A => IStream[B]): IStream[B] =
        foldRight(fa, empty[B])((h, t) => plus(f(h), t))

      def plus[A](a: IStream[A], b: =>IStream[A]): IStream[A] =
        a.foldRightByName(b)(Lazy.cons)
      def empty[A]: IStream[A] = IStream.empty[A]

      def isEmpty[A](fa: IStream[A]): Boolean = fa.isInstanceOf[Nil[_]]

      override def foldMap[A, B](fa: IStream[A])(f: A => B)(implicit F: Monoid[B]): B =
        foldRight(fa, F.zero)((a, b) => F.append(f(a), b))

      override def foldRight[A, B](fa: IStream[A], z: =>B)(
        f: (A, =>B) => B
      ): B =
        fa match {
          case _: Nil[_]        => z
          case Cons(head, tail) => f(head.value, foldRight(tail.value, z)(f))
        }

      override def foldLeft[A, B](fa: IStream[A], z: B)(f: (B, A) => B): B = {
        @tailrec def loop(t: IStream[A], acc: B): B = t match {
          case _: Nil[_]        => acc
          case Cons(head, tail) => loop(tail.value, f(acc, head.value))
        }
        loop(fa, z)
      }

      def traverseImpl[G[_], A, B](
        fa: IStream[A]
      )(f: A => G[B])(implicit G: Applicative[G]): G[IStream[B]] =
        fa.foldRightByName(G.point(empty[B]))(
          (x, ys) => G.apply2(f(x), ys)(_ !: _)
        )

      // wtf is traversal all about?
      override def traverse[G[_]: Applicative, A, B](fa: IStream[A])(f: A => G[B]): G[IStream[B]] = traverseImpl(fa)(f)

    }

  // it would be more in keeping with the data structure's objectives to
  // Align.align and compare elements, to avoid a full traversal...
  implicit def equal[A: Equal]: Equal[IStream[A]] = Equal[IList[A]].contramap(instances.toIList(_))

}
