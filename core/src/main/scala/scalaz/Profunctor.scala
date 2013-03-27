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
        UpStar(h compose f)
      def mapsnd[A,B,C](h: UpStar[F,A,B])(f: B => C): UpStar[F,A,C] =
        UpStar(a => Functor[F].map(h(a))(f))
    }

  implicit def downStarProfunctor[F[_]:Functor]: Profunctor[({type λ[α,β]=DownStar[F,α,β]})#λ] =
    new Profunctor[({type λ[α,β]=DownStar[F,α,β]})#λ] {
      def mapfst[A,B,C](h: DownStar[F,A,B])(f: C => A): DownStar[F,C,B] =
        DownStar(fa => h(Functor[F].map(fa)(f)))
      def mapsnd[A,B,C](h: DownStar[F,A,B])(f: B => C): DownStar[F,A,C] =
        DownStar(f compose h)
    }

  implicit def upStarFunctor[F[_]:Functor,D]: Functor[({type λ[α]=UpStar[F,D,α]})#λ] =
    new Functor[({type λ[α]=UpStar[F,D,α]})#λ] {
      def map[A,B](m: UpStar[F,D,A])(f: A => B) = upStarProfunctor[F].mapsnd(m)(f)
    }

  implicit def downStarFunctor[F[_]:Functor,D]: Functor[({type λ[α]=DownStar[F,D,α]})#λ] =
    new Functor[({type λ[α]=DownStar[F,D,α]})#λ] {
      def map[A,B](f: DownStar[F,D,A])(k: A => B) = DownStar(k compose f)
    }
  ////
}
