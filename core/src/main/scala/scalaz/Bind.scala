package scalaz

////
/**
 * An [[scalaz.Apply]] functor, where a lifted function can introduce
 * new values _and_ new functor context to be incorporated into the
 * lift context.  The essential new operation of [[scalaz.Monad]]s.
 */
////
trait Bind[F[_]] extends Apply[F] { self =>
  ////

  /** Equivalent to `join(map(fa)(f))`. */
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = bind(f)(f => map(fa)(f))

  /** Sequence the inner `F` of `FFA` after the outer `F`, forming a
   * single `F[A]`. */
  def join[A](ffa: F[F[A]]) = bind(ffa)(a => a)

  // derived functions
  import Liskov._

  /**
   * `if` lifted into a binding.  Unlike `lift3((t,c,a)=>if(t)c else
   * a)`, this will only include context from the chosen of `ifTrue`
   * and `ifFalse`, not the other.
   */
  def ifM[B](value: F[Boolean], ifTrue: => F[B], ifFalse: => F[B]): F[B] =
    bind(value)(x => if (x) ifTrue else ifFalse)

  ////
  val bindSyntax = new scalaz.syntax.BindSyntax[F] {}
}

object Bind {
  @inline def apply[F[_]](implicit F: Bind[F]): Bind[F] = F

  ////

  ////
}

