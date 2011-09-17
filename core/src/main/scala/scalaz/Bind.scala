package scalaz

trait BindLike[F[_]] extends ApplyLike[F] { self =>
  ////

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(f => map(fa)(f))

  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)

  ////
  val bindSyntax = new scalaz.syntax.BindSyntax[F] {}
}

////
/**
 *
 */
////
trait Bind[F[_]] extends BindLike[F] {
  self : ApplyInstance[F] =>

  implicit val bindParents: ApplyInstance[F] = this
}

object Bind {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F

  ////

  ////
}

trait BindInstance[F[_]] extends Bind[F] with ApplyInstance[F]
