package scalaz
package data

import language.implicitConversions

/** A universally quantified function, usually written as `F ~> G`,
  * for symmetry with `A => B`.
  *
  * All `FunctionH[F, G]` values, with `F` and `G` functors, form functor
  * homomorphisms (natural transformations) from `F` to `G` by a free theorem.
  */
trait FunctionH[-F[_], +G[_]] {
  self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: E ~> F): E ~> G = λ[E ~> G](
    ea => self(f(ea))
  )

  def andThen[H[_]](f: G ~> H): F ~> H =
    f compose self
}

trait FunctionHFunctions {
  /** A universally quantified identity function */
  def id[F[_]] =
    λ[F ~> F](fa => fa)

  /** Reify a `NaturalTransformation`. */
  implicit def natToFunction[F[_], G[_], A](t: F ~> G): F[A] => G[A] = fa => t(fa)

  private object MkFunctionH {
    val unsafe: MkFunctionH = new MkFunctionH
  }

  /**
    * Use to create a FunctionH from a quantified method:
    * `FunctionH.mk[List, Option](_.headOption)`
    */
  def mk: MkFunctionH = MkFunctionH.unsafe
}

// credit to Alex Konovalov for this trick: undefined type members
// with unstable prefixes, to encode universal quantification
private[data] final class MkFunctionH(val dummy: Boolean = true) extends AnyVal {
  type T

  def apply[F[_], G[_]](f: F[T] => G[T]): FunctionH[F, G] = new FunctionH[F, G] {
    def apply[A](fa: F[A]): G[A] = f(fa.asInstanceOf[F[T]]).asInstanceOf[G[A]]
  }
}

object FunctionH extends FunctionHFunctions

