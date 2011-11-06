package scalaz

trait Applicative[F[_]] extends Apply[F] with Pointed[F] { self =>
  ////

  // derived functions
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(point(f))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    ap2(fa, fb)(point(f))

  // impls of sequence, traverse, etc

  def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(f)(this)

  def sequence[A, G[_]: Traverse](as: G[F[A]]): F[G[A]] =
    traverse(as)(a => a)

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
  @inline def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  ////
  type Of[FA] = UnpackMClass[Applicative, FA]

  ////
}

