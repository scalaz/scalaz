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
  * The pattern for `ContravariantCoyoneda` over `F`s like `Order` of
  * running `k` across the list, doing the sort, and then dropping the
  * `k` value is encoded in the `schwartzian*` members.  Their use
  * derives from their type.  See the test "Schwartzian-transformed
  * sort equals normal sort" in `ContravariantCoyonedaTest.scala` for
  * a demonstration.
  *
  * As `ContravariantCoyoneda(identity)(o).unlift` = `o`, further
  * factoring can occur as follows, for free:
  *
  * {{{
  * ContravariantCoyoneda(f)(o contramap g) =
  *   ContravariantCoyoneda(g compose f)(o)
  * }}}
  *
  * @see http://hackage.haskell.org/package/kan-extensions-4.0.1/docs/Data-Functor-Contravariant-Coyoneda.html
  */
sealed abstract class ContravariantCoyoneda[F[_], A] {
  /** The pivot between `fi` and `k`, usually existential. */
  type I

  /** The underlying value. */
  implicit val fi: F[I]

  /** The transformer function, to be lifted into `F` by `run`. */
  val k: A => I

  import ContravariantCoyoneda.{Aux, apply}

  /** Converts to `F[A]` given that `F` is a contravariant. */
  implicit final def run(implicit F: Contravariant[F]): F[A] =
    F.contramap(fi)(k)

  @inline final def schwartzianPre: A => (I, A) = a => (k(a), a)
  @inline final def schwartzianPost: ((I, A)) => A = _._2
  @inline final implicit def schwartzianOrder(implicit F: Contravariant[F])
      : F[(I, A)] = F.contramap(fi)(_._1)

  /** Simple function composition. Allows map fusion without touching
    * the underlying `F`.
    */
  final def contramap[B](f: B => A): Aux[F, B, I] = apply(k compose f)

  /** Natural transformation. */
  final def trans[G[_]](f: F ~> G): Aux[G, A, I] = apply(k)(f(fi))
}

sealed abstract class ContravariantCoyonedaInstances {
  import ContravariantCoyoneda.ContravariantCoyonedaF

  /** `ContravariantCoyoneda[F,_]` is a contravariant functor for any
    * `F`.
    */
  implicit def contravariantCoyonedaContravariant[F[_]]: Contravariant[ContravariantCoyonedaF[F]#A] =
    new Contravariant[ContravariantCoyonedaF[F]#A] {
      def contramap[A, B](fa: ContravariantCoyoneda[F, A])(f: B => A) =
        fa contramap f
    }
}

object ContravariantCoyoneda extends ContravariantCoyonedaInstances {
  /** Lift the `I` type member to a parameter.  It is usually more
    * convenient to use `Aux` than a structural type.
    */
  type Aux[F[_], A, B] = ContravariantCoyoneda[F, A] {type I = B}

  /** Curried `ContravariantCoyoneda` type constructor. */
  type ContravariantCoyonedaF[F[_]] = ({type A[α] = ContravariantCoyoneda[F, α]})

  /** See `by` method. */
  final class By[F[_]] {
    @inline def apply[A, B](k: A => B)(implicit F: F[B]): Aux[F, A, B] =
      ContravariantCoyoneda(k)
  }

  /** Partial application of type parameters to `apply`.  It is often
    * more convenient to invoke `ContravariantCoyoneda.by[F]{x: X =>
    * ...}` then `ContravariantCoyoneda[...]{x: X => ...}`.
    */
  @inline def by[F[_]]: By[F] = new By[F]

  /** Like `lift(F).contramap(_k)`. */
  def apply[F[_], A, B](_k: A => B)(implicit F: F[B]): Aux[F, A, B] =
    new ContravariantCoyoneda[F, A]{
      type I = B
      val k = _k
      val fi = F
    }

  /** `F[A]` converts to `ContravariantCoyoneda[F,A]` for any `F`. */
  def lift[F[_], A](implicit F: F[A]): ContravariantCoyoneda[F, A] =
    apply(conforms[A])(F)

  import Isomorphism._

  def iso[F[_]: Contravariant]: ContravariantCoyonedaF[F]#A <~> F =
    new IsoFunctorTemplate[ContravariantCoyonedaF[F]#A, F] {
      def from[A](fa: F[A]) = lift(fa)
      def to[A](fa: ContravariantCoyoneda[F, A]) = fa.run
    }
}
