package scalaz

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

    private def swapTuple[X,Y]: Tuple2[X,Y] => Tuple2[Y,X] = _.swap

    /** first' == dimap swap swap . second' */
    def firstIsSwappedSecond[A, B, C](fab: A =>: B)(implicit E: Equal[(A, C) =>: (B, C)]): Boolean = {
      val fa: (C, A) =>: (C, B) = second(fab)
      E.equal(first(fab), dimap(fa)(swapTuple[A,C])(swapTuple[C,B]))
    }

    /** second' == dimap swap swap . first' */
    def secondIsSwappedFirst[A, B, C](fab: A =>: B)(implicit E: Equal[(C, A) =>: (C, B)]): Boolean = {
      val fa: (A, C) =>: (B, C) = first(fab)
      E.equal(second(fab),dimap(fa)(swapTuple[C,A])(swapTuple[B,C]))
    }

    /** lmap fst == rmap fst . first' */
    def mapfstEqualsFirstAndThenMapsnd[A, B, C](fab: A =>: B)(implicit E: Equal[(A,C) =>: B]): Boolean = {
      val fa: (A,C) =>: (B, C) = first(fab)
      E.equal(mapfst(fab)(_._1), mapsnd(fa)(_._1))
    }

    /** lmap snd == rmap snd . second' */
    def mapfstEqualsSecondAndThenMapsnd[A, B, C](fab: A =>: B)(implicit E: Equal[(C,A) =>: B]): Boolean = {
      val fa: (C,A) =>: (C, B)  = second(fab)
      E.equal(mapfst(fab)(_._2), mapsnd(fa)(_._2))
    }

    private def assoc[A,B,C]: (((A,B), C)) => (A, (B,C))  = { case ((a,c),d) => (a,(c,d)) }
    private def unassoc[A,B,C]: ((A, (B,C))) => ((A,B), C) = { case (a,(c,d)) => ((a,c),d) }

    /** first' . first' == dimap assoc unassoc . first' where
      *   assoc ((a,b),c) = (a,(b,c))
      *   unassoc (a,(b,c)) = ((a,b),c)
      */
    def firstFirstIsDimap[A,B,C,D](fab: A =>: B)(implicit E: Equal[((A,C), D) =>: ((B,C), D)]): Boolean = {
      val l1: (A, C) =>: (B, C) = first(fab)
      val l2: ((A,C), D) =>: ((B,C), D) = first(l1)

      val r1: (A, (C,D)) =>: (B, (C,D)) = first(fab)
      val r2: ((A,C) ,D) =>: ((B,C), D) = dimap[(A, (C,D)),(B,(C,D)),((A,C),D),((B, C),D)](r1)(assoc)(unassoc)
      E.equal(l2, r2)
    }

    /** second' . second' == dimap unassoc assoc . second' where
      *   assoc ((a,b),c) = (a,(b,c))
      *   unassoc (a,(b,c)) = ((a,b),c)
      */
    def secondSecondIsDimap[A,B,C,D](fab: A =>: B)(implicit E: Equal[(D, (C,A)) =>: (D, (C,B))]): Boolean = {
      val l1: (C, A) =>: (C, B) = second(fab)
      val l2: (D, (C,A)) =>: (D, (C,B)) = second(l1)

      val r1: ((D,C), A) =>: ((D,C), B) = second(fab)
      val r2: (D, (C,A)) =>: (D, (C,B)) = dimap[((D,C), A),((D,C), B),(D, (C,A)),(D, (C,B))](r1)(unassoc)(assoc)
      E.equal(l2, r2)
    }
  }

  def strongLaw: StrongLaws = new StrongLaws {}

  ////
  val strongSyntax = new scalaz.syntax.StrongSyntax[=>:] { def F = Strong.this }
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
