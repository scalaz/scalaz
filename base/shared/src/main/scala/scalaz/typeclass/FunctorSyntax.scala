package scalaz
package typeclass

import scala.language.experimental.macros

trait FunctorSyntax {
  implicit final class ToFunctorOps[F[_], A](self: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = macro meta.Ops.f_1

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
    def fmap[B](f: A => B): F[B] = F.map[A, B](self)(f)
    def void: F[Unit]            = F.map[A, Unit](self)(_ => ())
  }
}
