package scalaz

trait Applicative[F[_]] extends Apply[F] with Pointed[F] { self =>
  ////

  // derived functions
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap2(fa, fb)(pure(f))

  // impls of sequence, traverse, etc

  /**The composition of Applicatives F and G, [x]F[G[x]], is an Applicative */
  def compose[G[_]](G0: Applicative[G]): Applicative[({type λ[α] = F[G[α]]})#λ] = new CompositionApplicative[F, G] {
    implicit def F: Applicative[F] = self

    implicit def G: Applicative[G] = G0
  }

  /**The product of Applicatives F and G, [x](F[x], G[x]]), is an Applicative */
  def product[G[_]](implicit G0: Applicative[G]): Applicative[({type λ[α] = (F[α], G[α])})#λ] = new ProductApplicative[F, G] {
    implicit def F: Applicative[F] = self

    implicit def G: Applicative[G] = G0
  }

  ////
  val applicativeSyntax = new scalaz.syntax.ApplicativeSyntax[F] {}
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  ////

  ////
}
