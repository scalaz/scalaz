package scalaz

////
/**
 * Profunctors are covariant on the right and contravariant on the left.
 */
////
trait Profunctor[=>:[_, _]]  { self =>
  ////
  /** Contramap on `A`. */
  def mapfst[A, B, C](fab: (A =>: B))(f: C => A): (C =>: B)

  /** Functor map on `B`. */
  def mapsnd[A, B, C](fab: (A =>: B))(f: B => C): (A =>: C)

  /** Functor map on `A` and `B`. */
  def dimap[A, B, C, D](fab: (A =>: B))(f: C => A)(g: B => D): (C =>: D) =
    mapsnd(mapfst(fab)(f))(g)

  protected[this] trait SndCovariant[C] extends Functor[=>:[C, *]] {
    override def map[A, B](fa: C =>: A)(f: A => B) = mapsnd(fa)(f)
  }

  def invariantFunctor: InvariantFunctor[λ[α => α =>: α]] =
    new InvariantFunctor[λ[α => α =>: α]] {
      def xmap[A, B](ma: A =>: A, f: A => B, g: B => A) =
        mapsnd(mapfst(ma)(g))(f)
    }

  def covariantInstance[C]: Functor[=>:[C, *]] =
    new SndCovariant[C]{}

  def contravariantInstance[C]: Contravariant[=>:[*, C]] =
    new Contravariant[=>:[*, C]] {
      def contramap[A, B](fa: A =>: C)(f: B => A): (B =>: C) =
        mapfst(fa)(f)
    }

  ////
  val profunctorSyntax: scalaz.syntax.ProfunctorSyntax[=>:] =
    new scalaz.syntax.ProfunctorSyntax[=>:] { def F = Profunctor.this }
}

object Profunctor {
  @inline def apply[F[_, _]](implicit F: Profunctor[F]): Profunctor[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Profunctor[G]): Profunctor[F] =
    new IsomorphismProfunctor[F, G] {
      override def G: Profunctor[G] = E
      override def iso: F <~~> G = D
    }

  ////
  sealed trait UpStarF
  type UpStar[F[_], D, C] = (D => F[C]) @@ UpStarF
  val UpStar = Tag.of[UpStarF]

  sealed trait DownStarF
  type DownStar[F[_], D, C] = (F[D] => C) @@ DownStarF
  val DownStar = Tag.of[DownStarF]

  implicit def upStarProfunctor[F[_]: Functor]: Profunctor[UpStar[F, *, *]] =
    new Profunctor[UpStar[F, *, *]] {
      def mapfst[A, B, C](h: UpStar[F, A, B])(f: C => A): UpStar[F, C, B] =
        UpStar(Tag unwrap h compose f)
      def mapsnd[A, B, C](h: UpStar[F, A, B])(f: B => C): UpStar[F, A, C] =
        UpStar(a => Functor[F].map(Tag.unwrap(h)(a))(f))
    }

  implicit def downStarProfunctor[F[_]: Functor]: Profunctor[DownStar[F, *, *]] =
    new Profunctor[DownStar[F, *, *]] {
      def mapfst[A, B, C](h: DownStar[F, A, B])(f: C => A): DownStar[F, C, B] =
        DownStar(fa => Tag.unwrap(h)(Functor[F].map(fa)(f)))
      def mapsnd[A, B, C](h: DownStar[F, A, B])(f: B => C): DownStar[F, A, C] =
        DownStar(f compose Tag.unwrap(h))
    }

  implicit def upStarFunctor[F[_]: Functor, D]: Functor[UpStar[F, D, *]] =
    new Functor[UpStar[F, D, *]] {
      def map[A, B](m: UpStar[F, D, A])(f: A => B) =
        upStarProfunctor[F].mapsnd(m)(f)
    }

  implicit def downStarFunctor[F[_], D]: Functor[DownStar[F, D, *]] =
    new Functor[DownStar[F, D, *]] {
      def map[A, B](f: DownStar[F, D, A])(k: A => B) =
        DownStar(k compose Tag.unwrap(f))
    }
  ////
}
