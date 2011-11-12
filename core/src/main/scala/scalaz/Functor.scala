package scalaz

////
/**
 *
 */
////
trait Functor[F[_]]  { self =>
  ////

  def map[A, B](fa: F[A])(f: A => B): F[B]

  // derived functions
  def apply[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def strengthL[A, B](a: A, f: F[B]): F[(A, B)] = map(f)(b => (a, b))

  def strengthR[A, B](f: F[A], b: B): F[(A, B)] = map(f)(a => (a, b))

  def mapply[A, B](a: A)(f: F[A => B]): F[B] = map(f)((ff: A => B) => ff(a))

  /**The composition of Functors F and G, [x]F[G[x]], is an Functors */
  def compose[G[_]](G0: Functor[G]): Functor[({type λ[α] = F[G[α]]})#λ] = new CompositionFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Functors F and G, [x](F[x], G[x]]), is an Functor */
  def product[G[_]](implicit G0: Functor[G]): Functor[({type λ[α] = (F[α], G[α])})#λ] = new ProductFunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  ////
  val functorSyntax = new scalaz.syntax.FunctorSyntax[F] {}
}

object Functor {
  @inline def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  ////

  ////
}

