package scalaz

/** Decomposition of `fi.contramap(k)` into its components, as it is
  * frequently convenient to apply `k` separately from sorting or
  * whatever process with `fi`, even when `B` is unknown, which is
  * very common.
  *
  * This is isomorphic to `F` as long as `F` itself is a contravariant
  * functor.  The homomorphism from `F[A]` to
  * `ContravariantCoyoneda[F,A]` exists even when `F` is not a
  * contravariant functor.
  *
  * See `ContravariantCoyonedaUsage.scala` in the scalaz source tree
  * for an interesting usage demonstration.
  *
  * As `ContravariantCoyoneda(o)(identity).unlift` = `o`, further
  * factoring can occur as follows, for free:
  *
  * {{{
  * ContravariantCoyoneda(o contramap g)(f).unlift =
  *   ContravariantCoyoneda(o)(g compose f).unlift
  * }}}
  *
  * @see [[http://hackage.haskell.org/package/kan-extensions-4.0.1/docs/Data-Functor-Contravariant-Coyoneda.html]]
  */
sealed abstract class ContravariantCoyoneda[F[_], A] {
  /** The pivot between `fi` and `k`, usually existential. */
  type I

  /** The underlying value. */
  val fi: F[I]

  /** The transformer function, to be lifted into `F` by `run`. */
  val k: A => I

  import ContravariantCoyoneda.{Aux, apply}

  /** Converts to `F[A]` given that `F` is a contravariant. */
  final def run(implicit F: Contravariant[F]): F[A] =
    F.contramap(fi)(k)

  /** Alias for `run`. */
  @inline final def unlift(implicit F: Contravariant[F]): F[A] = run

  /** Simple function composition. Allows map fusion without touching
    * the underlying `F`.
    */
  final def contramap[B](f: B => A): Aux[F, B, I] = apply(fi)(k compose f)

  /** Natural transformation. */
  final def trans[G[_]](f: F ~> G): Aux[G, A, I] = apply(f(fi))(k)
}

sealed abstract class ContravariantCoyonedaInstances {
  /** `ContravariantCoyoneda[F,_]` is a contravariant functor for any
    * `F`.
    */
  implicit def contravariantCoyonedaContravariant[F[_]]: Contravariant[ContravariantCoyoneda[F, ?]] =
    new Contravariant[ContravariantCoyoneda[F, ?]] {
      def contramap[A, B](fa: ContravariantCoyoneda[F, A])(f: B => A) =
        fa contramap f
    }
}

object ContravariantCoyoneda extends ContravariantCoyonedaInstances {
  /** Lift the `I` type member to a parameter.  It is usually more
    * convenient to use `Aux` than a structural type.
    */
  type Aux[F[_], A, B] = ContravariantCoyoneda[F, A] {type I = B}

  /** See `by` method. */
  final class By[F[_]] {
    @inline def apply[A, B](k: A => B)(implicit F: F[B]): Aux[F, A, B] =
      ContravariantCoyoneda(F)(k)
  }

  /** Partial application of type parameters to `apply`.  It is often
    * more convenient to invoke `ContravariantCoyoneda.by[F]{x: X =>
    * ...}` then `ContravariantCoyoneda[...](...){x: X => ...}`.
    */
  @inline def by[F[_]]: By[F] = new By[F]

  /** Like `lift(fa).contramap(_k)`. */
  def apply[F[_], A, B](fa: F[B])(_k: A => B): Aux[F, A, B] =
    new ContravariantCoyoneda[F, A]{
      type I = B
      val k = _k
      val fi = fa
    }

  /** `F[A]` converts to `ContravariantCoyoneda[F,A]` for any `F`. */
  def lift[F[_], A](fa: F[A]): ContravariantCoyoneda[F, A] =
    apply(fa)(identity[A])

  import Isomorphism._

  def iso[F[_]: Contravariant]: ContravariantCoyoneda[F, ?] <~> F =
    new IsoFunctorTemplate[ContravariantCoyoneda[F, ?], F] {
      def from[A](fa: F[A]) = lift(fa)
      def to[A](fa: ContravariantCoyoneda[F, A]) = fa.run
    }
}
