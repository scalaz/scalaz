package scalaz

////
import scalaz.Id.Id

/**
 * A [[scalaz.Traverse]] where `traverse` is total over
 * [[scalaz.Apply]]s.  That is, `toList` cannot return an empty list.
 */
////
trait Traverse1[F[_]] extends Traverse[F] with Foldable1[F] { self =>
  ////

  /**The product of Traverse1 `F` and `G`, `[x](F[x], G[x]])`, is a Traverse1 */
  def product[G[_]](implicit G0: Traverse1[G]): Traverse1[({type λ[α] = (F[α], G[α])})#λ] = new ProductTraverse1[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Traverse1 `F` and Traverse `G`, `[x](F[x], G[x]])`, is a Traverse1 */
  def product0[G[_]](implicit G0: Traverse[G]): Traverse1[({type λ[α] = (F[α], G[α])})#λ] =
    new ProductTraverse1L[F, G] {
      def F = self
      def G = G0
    }

  /** Transform `fa` using `f`, collecting all the `G`s with `ap`. */
  def traverse1Impl[G[_]:Apply,A,B](fa: F[A])(f: A => G[B]): G[F[B]]

  // derived functions
  override def traverseImpl[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    traverse1Impl(fa)(f)

  override def foldMap1[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): B =
    foldLeft1(traverse1Impl[Id, A, B](fa)(a => f(a)))(F.append(_, _))

  def traverse1[G[_], A, B](fa: F[A])(f: A => G[B])(implicit a: Apply[G]): G[F[B]] =
    traverse1Impl(fa)(f)

  def sequence1[G[_]:Apply,A](fga: F[G[A]]): G[F[A]] =
    traverse1Impl[G, G[A], A](fga)(identity)

  ////
  val traverse1Syntax = new scalaz.syntax.Traverse1Syntax[F] { def F = Traverse1.this }
}

object Traverse1 {
  @inline def apply[F[_]](implicit F: Traverse1[F]): Traverse1[F] = F

  ////

  ////
}
