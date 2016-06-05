package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scala.annotation.tailrec

trait ForallM[P[_[_]]] {
  def apply[F[_]: Monad]: P[F]
}

abstract class EnumeratorP[E, F[_]] { self =>
  def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[E, G]

  def map[B](f: E => B): EnumeratorP[B, F] =
    new EnumeratorP[B, F] {
      def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
        import MO._
        self[G].map[B](f)
      }
    }

  def flatMap[B](f: E => EnumeratorP[B, F]) =
    new EnumeratorP[B, F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[B, G] = {
        import MO._
        self[G].flatMap[B](e => f(e).apply[G])
      }
    }

  def collect[B](pf: PartialFunction[E, B]) =
    new EnumeratorP[B, F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[B, G] = {
        import MO._
        self[G].collect[B](pf)
      }
    }

  def uniq(implicit ord: Order[E]) =
    new EnumeratorP[E, F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[E, G] = {
        import MO._
        self[G].uniq
      }
    }

  def zipWithIndex =
    new EnumeratorP[(E, Long), F] {
      def apply[G[_]](implicit MO: G |>=| F): EnumeratorT[(E, Long), G] = {
        import MO._
        self[G].zipWithIndex
      }
    }

  def :^[B](other: EnumeratorP[B, F]): EnumeratorP[(E, B), F] =
    new EnumeratorP[(E, B), F] {
      def apply[G[_]](implicit MO: G |>=| F) = {
        import MO._
        self[G].cross[B](other[G])
      }
    }

  def ^:[B](other: EnumeratorP[B, F]): EnumeratorP[(E, B), F] =
    new EnumeratorP[(E, B), F] {
      def apply[G[_]](implicit MO: G |>=| F) = {
        import MO._
        self[G].cross[B](other[G])
      }
    }

  def join(other: EnumeratorP[E, F])(implicit order: Order[E], m: Monad[F]): EnumeratorP[(E, E), F] =
    EnumeratorP.joinE[E, E, F](m, order.order).apply(self, other)

  def merge(other: EnumeratorP[E, F])(implicit ord: Order[E], m: Monad[F]) =
    EnumeratorP.mergeE[E, F].apply(self, other)
}

trait EnumeratorPFunctions {
  def empty[E, F[_]]: EnumeratorP[E, F] = new EnumeratorP[E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
      import MO._
      EnumeratorT.empty[E, G]
    }
  }

  def perform[E, F[_], B](f: F[B]): EnumeratorP[E, F] = new EnumeratorP[E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
      import MO._
      EnumeratorT.perform[E, G, B](MO.promote(f))
    }
  }

  def enumPStream[E, F[_]: Monad](xs : Stream[E]): EnumeratorP[E, F] = new EnumeratorP[E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[E, G] = {
      import MO._
      enumStream[E, G](xs)
    }
  }

  def liftE2[J, K, I, F[_]](e2t: ForallM[λ[β[_] => Enumeratee2T[J, K, I, β]]]): (EnumeratorP[J, F], EnumeratorP[K, F]) => EnumeratorP[I, F] = {
    (e1: EnumeratorP[J, F], e2: EnumeratorP[K, F]) => new EnumeratorP[I, F] {
      def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[I, G] =
        new EnumeratorT[I, G] {
          import MO._
          implicit val IOrd = MO.transform[λ[(β[_], α) => IterateeT[K, β, α]]]
          lazy val enum1 = e1[IterateeT[K, G, ?]]
          lazy val enum2 = e2[G]

          def apply[A] = {
            (step: StepT[I, G, A]) => iterateeT(((e2t[G].apply(step) &= enum1).run &= enum2).run)
          }
        }
    }
  }

  def cogroupE[J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering) = liftE2[J, K, Either3[J, (J, K), K], F] {
    new ForallM[λ[β[_] => Enumeratee2T[J, K, Either3[J, (J, K), K], β]]] {
      def apply[G[_]: Monad] = cogroupI[J, K, G]
    }
  }

  def joinE[J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering) = liftE2[J, K, (J, K), F] {
    new ForallM[λ[β[_] => Enumeratee2T[J, K, (J, K), β]]] {
      def apply[G[_]: Monad] = joinI[J, K, G]
    }
  }

  def mergeE[E: Order, F[_]: Monad] = liftE2[E, E, E, F] {
    new ForallM[λ[β[_] => Enumeratee2T[E, E, E, β]]] {
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
        def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
          import MO._
          EnumeratorT.enumeratorTMonoid[E, G].append(f1[G], f2[G])
        }
      }
  }
}

object EnumeratorP extends EnumeratorPInstances with EnumeratorPFunctions

// vim: set ts=4 sw=4 et:
