package scalaz

////
import scalaz.Id.Id

/**
 *
 */
////
trait Traverse1[F[_]] extends Traverse[F] with Foldable1[F] { self =>
  ////
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

