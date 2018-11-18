package scalaz

import scalaz.syntax.StrongSyntax

import scala.language.higherKinds

////
/**
 * Strength on a product.
 */
////
trait Strong[=>:[_, _]] extends Profunctor[=>:] { self =>
  ////
  def first[A, B, C](fa: A =>: B): (A, C) =>: (B, C)

  def second[A, B, C](fa: A =>: B): (C, A) =>: (C, B) = {
    dimap(first(fa): (A, C) =>: (B, C))((ac: (C,A)) => ac.swap)((bc: (B, C)) => bc.swap)
  }

  trait StrongLaws extends ProfunctorLaw {

    def swapTuple[X,Y]: Tuple2[X,Y] => Tuple2[Y,X] = _.swap

    def firstIsSwappedSecond[A, B, C](fab: A =>: B)(implicit E: Equal[(A, C) =>: (B, C)]): Boolean = {
      val fa: (C, A) =>: (C, B) = second(fab)
      E.equal(first(fab), dimap(fa)(swapTuple[A,C])(swapTuple[C,B]))
    }

    def secondIsSwappedFirst[A, B, C](fab: A =>: B)(implicit E: Equal[(C, A) =>: (C, B)]): Boolean = {
      val fa: (A, C) =>: (B, C) = first(fab)
      E.equal(second(fab),dimap(fa)(swapTuple[C,A])(swapTuple[B,C]))
    }

    def mapfstEqualsFirstAndThenMapsnd[A, B, C](fab: A =>: B)(implicit E: Equal[(A,C) =>: B]): Boolean = {
      val fa: (A,C) =>: (B, C) = first(fab)
      E.equal(mapfst(fab)(_._1), mapsnd(fa)(_._1))
    }

    def mapfstEqualsSecondAndThenMapsnd[A, B, C](fab: A =>: B)(implicit E: Equal[(C,A) =>: B]): Boolean = {
      val fa: (C,A) =>: (C, B)  = second(fab)
      E.equal(mapfst(fab)(_._2), mapsnd(fa)(_._2))
    }
  }

  def strongLaw: StrongLaws = new StrongLaws {}

  ////
  val strongSyntax: StrongSyntax[=>:] = new scalaz.syntax.StrongSyntax[=>:] { def F: Strong[=>:] = Strong.this }
}

object Strong {
  @inline def apply[F[_, _]](implicit F: Strong[F]): Strong[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Strong[G]): Strong[F] =
    new IsomorphismStrong[F, G] {
      override def G: Strong[G] = E
      override def iso: F <~~> G = D
    }

  ////

  def uncurry[F[_,_],A,B,C](fa: F[A, B => C])(implicit F: Strong[F]): F[(A,B),C] = {
    val fabc: F[(A, B), (B => C, B)] = F.first(fa)
    val f = (bc: (B => C, B)) => bc._1(bc._2)
    F.mapsnd(fabc)(f)
  }

  ////
}

trait IsomorphismStrong[F[_, _], G[_, _]] extends Strong[F] with IsomorphismProfunctor[F, G]{
  implicit def G: Strong[G]
  ////

  override def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)] =
    iso.from(G.first(iso.to(fa)))

  override def second[A, B, C](fa: F[A, B]): F[(C, A), (C, B)] =
    iso.from(G.second(iso.to(fa)))
  ////
}
