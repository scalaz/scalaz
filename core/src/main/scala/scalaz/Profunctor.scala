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

  protected[this] trait SndCovariant[C] extends Functor[({type λ[α] = C =>: α})#λ] {
    override def map[A, B](fa: C =>: A)(f: A => B) = mapsnd(fa)(f)
  }

  def invariantFunctor: InvariantFunctor[({type λ[α] = α =>: α})#λ] =
    new InvariantFunctor[({type λ[α] = α =>: α})#λ] {
      def xmap[A, B](ma: A =>: A, f: A => B, g: B => A) =
        mapsnd(mapfst(ma)(g))(f)
    }

  def covariantInstance[C]: Functor[({type λ[α] = C =>: α})#λ] =
    new SndCovariant[C]{}

  def contravariantInstance[C]: Contravariant[({type λ[α] = (α =>: C)})#λ] =
    new Contravariant[({type λ[α] = (α =>: C)})#λ] {
      def contramap[A, B](fa: A =>: C)(f: B => A): (B =>: C) =
        mapfst(fa)(f)
    }

  ////
  val profunctorSyntax = new scalaz.syntax.ProfunctorSyntax[=>:] { def F = Profunctor.this }
}

object Profunctor {
  @inline def apply[F[_, _]](implicit F: Profunctor[F]): Profunctor[F] = F

  ////
  sealed trait UpStarF
  type UpStar[F[_],D,C] = (D => F[C]) @@ UpStarF
  def UpStar[F[_],D,C](f: D => F[C]): UpStar[F,D,C] = Tag[D => F[C], UpStarF](f)

  sealed trait DownStarF
  type DownStar[F[_],D,C] = (F[D] => C) @@ DownStarF
  def DownStar[F[_],D,C](f: F[D] => C): DownStar[F,D,C] = Tag[F[D] => C, DownStarF](f)

  implicit def upStarProfunctor[F[_]:Functor]: Profunctor[({type λ[α,β]=UpStar[F,α,β]})#λ] =
    new Profunctor[({type λ[α,β] = UpStar[F,α,β]})#λ] {
      def mapfst[A,B,C](h: UpStar[F,A,B])(f: C => A): UpStar[F,C,B] =
        UpStar(Tag unwrap h compose f)
      def mapsnd[A,B,C](h: UpStar[F,A,B])(f: B => C): UpStar[F,A,C] =
        UpStar(a => Functor[F].map(Tag.unwrap(h)(a))(f))
    }

  implicit def downStarProfunctor[F[_]:Functor]: Profunctor[({type λ[α,β]=DownStar[F,α,β]})#λ] =
    new Profunctor[({type λ[α,β]=DownStar[F,α,β]})#λ] {
      def mapfst[A,B,C](h: DownStar[F,A,B])(f: C => A): DownStar[F,C,B] =
        DownStar(fa => Tag.unwrap(h)(Functor[F].map(fa)(f)))
      def mapsnd[A,B,C](h: DownStar[F,A,B])(f: B => C): DownStar[F,A,C] =
        DownStar(f compose Tag.unwrap(h))
    }

  implicit def upStarFunctor[F[_]:Functor,D]: Functor[({type λ[α]=UpStar[F,D,α]})#λ] =
    new Functor[({type λ[α]=UpStar[F,D,α]})#λ] {
      def map[A,B](m: UpStar[F,D,A])(f: A => B) = upStarProfunctor[F].mapsnd(m)(f)
    }

  implicit def downStarFunctor[F[_],D]: Functor[({type λ[α]=DownStar[F,D,α]})#λ] =
    new Functor[({type λ[α]=DownStar[F,D,α]})#λ] {
      def map[A,B](f: DownStar[F,D,A])(k: A => B) = DownStar[F,D,B](k compose Tag.unwrap(f))
    }
  ////
}
