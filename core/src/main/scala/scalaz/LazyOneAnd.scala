// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import scala.{ Boolean, Int, None, Some }

import scalaz.std.option.{ none, optionMonoid, some }

// WORKAROUND: https://github.com/scalaz/scalaz/issues/1516
final class LazyOneAnd[F[_], A](h: =>A, t: =>F[A]) {
  def head: A    = h
  def tail: F[A] = t
}
object LazyOneAnd {
  def apply[F[_], A](h: =>A, t: =>F[A]): LazyOneAnd[F, A] =
    new LazyOneAnd(h, t)

  implicit def Traverse1[F[_]](
    implicit F: Traverse[F]
  ): Traverse1[LazyOneAnd[F, ?]] =
    new Traverse1[LazyOneAnd[F, ?]] {
      def traverse1Impl[G[_], A, B](
        fa: LazyOneAnd[F, A]
      )(f: A => G[B])(implicit G: Apply[G]) =
        G.applyApplicative
          .traverse(fa.tail)(f andThen \/.left)(F)
          .fold(
            ftl =>
              G.apply2(f(fa.head), ftl) {
                case (h, t) => LazyOneAnd(h, t)
            },
            tl => G.map(f(fa.head))(LazyOneAnd(_, tl))
          )

      override def traverseImpl[G[_], A, B](
        fa: LazyOneAnd[F, A]
      )(f: A => G[B])(implicit G: Applicative[G]) =
        G.apply2(f(fa.head), F.traverseImpl(fa.tail)(f)(G)) {
          case (h, t) => LazyOneAnd(h, t)
        }

      override def traverseS[S, A, B](
        fa: LazyOneAnd[F, A]
      )(f: A => State[S, B]) =
        State { s: S =>
          val (s2, b)  = f(fa.head)(s)
          val (s3, bs) = F.traverseS(fa.tail)(f)(s2)
          (s3, LazyOneAnd(b, bs))
        }

      override def findLeft[A](fa: LazyOneAnd[F, A])(f: A => Boolean) =
        if (f(fa.head)) Some(fa.head) else F.findLeft(fa.tail)(f)

      override def findRight[A](fa: LazyOneAnd[F, A])(f: A => Boolean) =
        F.findRight(fa.tail)(f) match {
          case a @ Some(_) =>
            a
          case None =>
            if (f(fa.head)) Some(fa.head) else None
        }

      override def foldMap1[A, B: Semigroup](fa: LazyOneAnd[F, A])(f: A => B) =
        foldMap(fa)(a => some(f(a))) getOrElse f(fa.head)

      override def foldMapRight1[A, B](
        fa: LazyOneAnd[F, A]
      )(z: A => B)(f: (A, =>B) => B) =
        (F.foldRight(fa.tail, none[B])(
          (a, ob) => ob map (f(a, _)) orElse some(z(a))
        )
          map (f(fa.head, _)) getOrElse z(fa.head))

      override def foldMapLeft1[A, B](
        fa: LazyOneAnd[F, A]
      )(z: A => B)(f: (B, A) => B) =
        F.foldLeft(fa.tail, z(fa.head))(f)

      override def foldMap[A, B](
        fa: LazyOneAnd[F, A]
      )(f: A => B)(implicit M: Monoid[B]) =
        M.append(f(fa.head), F.foldMap(fa.tail)(f))

      override def foldRight[A, B](fa: LazyOneAnd[F, A],
                                   z: =>B)(f: (A, =>B) => B) =
        f(fa.head, F.foldRight(fa.tail, z)(f))

      override def foldLeft[A, B](fa: LazyOneAnd[F, A], z: B)(f: (B, A) => B) =
        F.foldLeft(fa.tail, f(z, fa.head))(f)

      override def traverseS_[S, A, B](
        fa: LazyOneAnd[F, A]
      )(f: A => State[S, B]) =
        State { s: S =>
          F.traverseS_(fa.tail)(f)(f(fa.head)(s)._1)
        }

      override def length[A](fa: LazyOneAnd[F, A]) = 1 + F.length(fa.tail)

      override def index[A](fa: LazyOneAnd[F, A], i: Int) =
        if (i == 0) Some(fa.head) else F.index(fa.tail, i - 1)

      override def toVector[A](fa: LazyOneAnd[F, A]) =
        fa.head +: F.toVector(fa.tail)

      override def toList[A](fa: LazyOneAnd[F, A]) =
        fa.head :: F.toList(fa.tail)

      override def toIList[A](fa: LazyOneAnd[F, A]) =
        fa.head :: F.toIList(fa.tail)

      override def toSet[A](fa: LazyOneAnd[F, A]) =
        F.toSet(fa.tail) + fa.head

      override def toStream[A](fa: LazyOneAnd[F, A]) =
        fa.head #:: F.toStream(fa.tail)

      override def toEphemeralStream[A](fa: LazyOneAnd[F, A]) =
        EphemeralStream.cons(fa.head, F.toEphemeralStream(fa.tail))

      override def all[A](fa: LazyOneAnd[F, A])(f: A => Boolean) =
        f(fa.head) && F.all(fa.tail)(f)

      override def any[A](fa: LazyOneAnd[F, A])(f: A => Boolean) =
        f(fa.head) || F.any(fa.tail)(f)
    }

}
