package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scala.annotation.tailrec

trait ForallM[P[_[_]]] {
  def apply[F[_]: Monad]: P[F]
}

abstract class EnumeratorP[E, F[_]] { self =>
  def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[E, G]

  def map[B](f: E => B): EnumeratorP[B, F] =
    new EnumeratorP[B, F] {
      def apply[G[_]: Monad](trans: F ~> G) =
        self[G](trans).map[B](f)
    }

  def flatMap[B](f: E => EnumeratorP[B, F]) =
    new EnumeratorP[B, F] {
      def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[B, G] =
        self[G](trans).flatMap[B](e => f(e)[G](trans))
    }

  def collect[B](pf: PartialFunction[E, B]) =
    new EnumeratorP[B, F] {
      def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[B, G] =
        self[G](trans).collect[B](pf)
    }

  def uniq(implicit ord: Order[E]) =
    new EnumeratorP[E, F] {
      def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[E, G] =
        self[G](trans).uniq
    }

  def zipWithIndex =
    new EnumeratorP[(E, Long), F] {
      def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[(E, Long), G] =
        self[G](trans).zipWithIndex
    }

  def :^[B](other: EnumeratorP[B, F]): EnumeratorP[(E, B), F] =
    new EnumeratorP[(E, B), F] {
      def apply[G[_]: Monad](trans: F ~> G) =
        self[G](trans).cross[B](other[G](trans))
    }

  def ^:[B](other: EnumeratorP[B, F]): EnumeratorP[(E, B), F] =
    new EnumeratorP[(E, B), F] {
      def apply[G[_]: Monad](trans: F ~> G) =
        self[G](trans).cross[B](other[G](trans))
    }

  def join(other: EnumeratorP[E, F])(implicit order: Order[E]): EnumeratorP[(E, E), F] =
    EnumeratorP.joinE[E, E, F](order.order).apply(self, other)

  def merge(other: EnumeratorP[E, F])(implicit ord: Order[E], m: Monad[F]): EnumeratorP[E, F] =
    EnumeratorP.mergeE[E, F].apply(self, other)
}

trait EnumeratorPFunctions {
  def empty[E, F[_]]: EnumeratorP[E, F] = new EnumeratorP[E, F] {
    def apply[G[_]: Monad](trans: F ~> G) =
      EnumeratorT.empty[E, G]
  }

  def perform[E, F[_], B](f: F[B]): EnumeratorP[E, F] = new EnumeratorP[E, F] {
    def apply[G[_]: Monad](trans: F ~> G) =
      EnumeratorT.perform[E, G, B](trans(f))
  }

  def enumPStream[E, F[_]: Monad](xs : Stream[E]): EnumeratorP[E, F] = new EnumeratorP[E, F] {
    def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[E, G] =
      enumStream[E, G](xs)
  }

  def liftE2[J, K, I, F[_]](e2t: ForallM[({type l[β[_]] = Enumeratee2T[J, K, I, β]})#l]): (EnumeratorP[J, F], EnumeratorP[K, F]) => EnumeratorP[I, F] = {
    (e1: EnumeratorP[J, F], e2: EnumeratorP[K, F]) => new EnumeratorP[I, F] {
      def apply[G[_]: Monad](trans: F ~> G): EnumeratorT[I, G] =
        new EnumeratorT[I, G] {
          val transIterateeT = new (F ~> IterateeT[K, G, *]) {
            def apply[A](f: F[A]): IterateeT[K, G, A] =
              MonadTrans[({type l[β[_], α] = IterateeT[K, β, α]})#l].liftM(trans(f))
          }
          lazy val enum1 = e1[IterateeT[K, G, *]](transIterateeT)
          lazy val enum2 = e2[G](trans)

          def apply[A] =
            (step: StepT[I, G, A]) => iterateeT(((e2t[G].apply(step) &= enum1).run &= enum2).run)
        }
    }
  }

  def cogroupE[J, K, F[_]](compare: (J, K) => Ordering): (EnumeratorP[J, F], EnumeratorP[K, F]) => EnumeratorP[Either3[J, (J, K), K], F] =
    liftE2[J, K, Either3[J, (J, K), K], F] {
      new ForallM[({type l[β[_]] = Enumeratee2T[J, K, Either3[J, (J, K), K], β]})#l] {
        def apply[G[_] : Monad] = cogroupI[J, K, G](compare)
      }
    }

  def joinE[J, K, F[_]](compare: (J, K) => Ordering): (EnumeratorP[J, F], EnumeratorP[K, F]) => EnumeratorP[(J, K), F] =
    liftE2[J, K, (J, K), F] {
      new ForallM[({type l[β[_]] = Enumeratee2T[J, K, (J, K), β]})#l] {
        def apply[G[_] : Monad] = joinI[J, K, G](compare)
      }
    }

  def mergeE[E: Order, F[_]: Monad]: (EnumeratorP[E, F], EnumeratorP[E, F]) => EnumeratorP[E, F] = liftE2[E, E, E, F] {
    new ForallM[({type l[β[_]] = Enumeratee2T[E, E, E, β]})#l] {
      def apply[G[_]: Monad] = mergeI[E, G]
    }
  }

  def mergeAll[E: Order, F[_]: Monad](enumerators: EnumeratorP[E, F]*): EnumeratorP[E, F] = {
    @tailrec def mergeOne(e: EnumeratorP[E, F], es: List[EnumeratorP[E, F]]): EnumeratorP[E, F] = es match {
      case x :: xs => mergeOne(e merge x, xs)
      case Nil => e
    }

    enumerators.toList match {
      case x :: xs => mergeOne(x, xs)
      case Nil => empty[E, F]
    }
  }
}

sealed abstract class EnumeratorPInstances {
  implicit def enumeratorPMonoid[E, F[_]]: Monoid[EnumeratorP[E, F]] = new Monoid[EnumeratorP[E, F]] {
    def zero = EnumeratorP.empty[E, F]
    def append(f1: EnumeratorP[E, F], f2: => EnumeratorP[E, F]) =
      new EnumeratorP[E, F] {
        def apply[G[_]: Monad](trans: F ~> G) =
          EnumeratorT.enumeratorTMonoid[E, G].append(f1[G](trans), f2[G](trans))
      }
  }
}

object EnumeratorP extends EnumeratorPInstances with EnumeratorPFunctions

// vim: set ts=4 sw=4 et:
