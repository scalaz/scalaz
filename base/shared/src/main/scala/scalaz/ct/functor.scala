package scalaz
package ct

import scala.language.experimental.macros

trait FunctorClass[F[_]] extends InvariantFunctorClass[F] {

  def map[A, B](ma: F[A])(f: A => B): F[B]

  final override def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B] = map(ma)(f)
}

trait FunctorFunctions {
  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)
}

trait FunctorSyntax {
  implicit final class ToFunctorOps[F[_], A](self: F[A]) {
    def map[B](f: A => B)(implicit ev: Functor[F]): F[B] = macro meta.Ops.i_1

    /**
     * Alias for [[map]], since [[map]] can't be injected as syntax if
     * the implementing type already had a built-in `.map` method.
     *
     * Example:
     * {{{
     * scala> import scalaz.Scalaz._
     *
     * scala> val m: Map[Int, String] = Map(1 -> "hi", 2 -> "there", 3 -> "you")
     *
     * scala> m.fmap(_ ++ "!")
     * res0: Map[Int,String] = Map(1 -> hi!, 2 -> there!, 3 -> you!)
     * }}}
     */
    def fmap[B](f: A => B)(implicit ev: Functor[F]): F[B] = ev.map[A, B](self)(f)
    def void(implicit ev: Functor[F]): F[Unit]            = ev.map[A, Unit](self)(_ => ())
  }
}
