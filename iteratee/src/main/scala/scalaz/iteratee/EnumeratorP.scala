package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scala.annotation.tailrec
import scalaz.syntax.Syntax.bind._
import scalaz.syntax.Syntax.order._

trait ForallM[P[_[_]]] {
  def apply[F[_]: Monad]: P[F]
}

abstract class EnumeratorP[X, E, F[_]] { self =>
  def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[X, E, G]

  def merge(other: EnumeratorP[X, E, F])(implicit ord: Order[E], m: Monad[F]) = 
    EnumeratorP.mergeE[X, E, F].apply(self, other)

  def map[B](f: E => B): EnumeratorP[X, B, F] = new EnumeratorP[X, B, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
      import MO._
      self[G].map(f)
    }
  }
}

trait EnumeratorPFunctions {
  def empty[X, E, F[_]]: EnumeratorP[X, E, F] = new EnumeratorP[X, E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]) = {
      import MO._
      EnumeratorT.empty[X, E, G]
    }
  }

  def enumPStream[X, E, F[_]: Monad](xs : Stream[E]): EnumeratorP[X, E, F] = new EnumeratorP[X, E, F] {
    def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[X, E, G] = {
      import MO._
      Iteratee.enumStream[X, E, G](xs)
    }
  }

  def liftE2[X, J, K, I, F[_]](e2t: ForallM[({type λ[β[_]] = Enumeratee2T[X, J, K, I, β]})#λ]): (EnumeratorP[X, J, F], EnumeratorP[X, K, F]) => EnumeratorP[X, I, F] = {
    (e1: EnumeratorP[X, J, F], e2: EnumeratorP[X, K, F]) => new EnumeratorP[X, I, F] {
      def apply[G[_]](implicit MO: MonadPartialOrder[G, F]): EnumeratorT[X, I, G] = 
        new EnumeratorT[X, I, G] {
          import MO._
          implicit val IOrd = MO.transform[({ type λ[β[_], α] = IterateeT[X, K, β, α] })#λ]
          lazy val enum1 = e1[({ type λ[α] = IterateeT[X, K, G, α]})#λ]
          lazy val enum2 = e2[G]

          def apply[A] = {
            (step: StepT[X, I, G, A]) => iterateeT(((e2t[G].apply(step) &= enum1).run(err _) &= enum2).run(x => MO.promote(Monad[F].point(serr(x)))))
          }
        }
    }
  }

  def cogroupE[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering) = liftE2[X, J, K, Either3[J, (J, K), K], F] {
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, J, K, Either3[J, (J, K), K], β]})#λ] {
      def apply[G[_]: Monad] = cogroupI[X, J, K, G]
    }
  }

  def matchE[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering) = liftE2[X, J, K, (J, K), F] { 
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, J, K, (J, K), β]})#λ] {
      def apply[G[_]: Monad] = matchI[X, J, K, G]
    }
  }

  def mergeE[X, E: Order, F[_]: Monad] = liftE2[X, E, E, E, F] { 
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, E, E, E, β]})#λ] {
      def apply[G[_]: Monad] = mergeI[X, E, G]
    }
  }

  def mergeAll[X, E: Order, F[_]: Monad](enumerators: EnumeratorP[X, E, F]*): EnumeratorP[X, E, F] = { 
    @tailrec def mergeOne(e: EnumeratorP[X, E, F], es: List[EnumeratorP[X, E, F]]): EnumeratorP[X, E, F] = es match {
      case x :: xs => mergeOne(e merge x, xs) 
      case Nil => e
    }   

    enumerators.toList match {
      case x :: xs => mergeOne(x, xs) 
      case Nil => empty[X, E, F]
    }   
  }
}

object EnumeratorP extends EnumeratorPFunctions

// vim: set ts=4 sw=4 et:
