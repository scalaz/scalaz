package scalaz

/** Decomposition of `fa.contramap(liftee)` into its components, as
  * it is frequently convenient to apply `liftee` separately from
  * sorting or whatever process with `fa`, even when `B` is
  * unknown, which is very common.
  *
  * The pattern for `ContravariantCoyoneda` over `F`s like `Order` of
  * running `liftee` across the list, doing the sort, and then
  * dropping the `liftee` value is encoded in the `schwartzian*`
  * members.  Their use derives from their type.  See the test
  * "Schwartzian-transformed sort equals normal sort" in
  * `ContravariantCoyonedaTest.scala` for a demonstration.
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
  type B
  val liftee: A => B
  implicit val fa: F[B]

  implicit final def unlift(implicit F: Contravariant[F]): F[A] =
    F.contramap(fa)(liftee)

  @inline final def schwartzianPre: A => (B, A) = a => (liftee(a), a)
  @inline final def schwartzianPost: ((B, A)) => A = _._2
  @inline final implicit def schwartzianOrder(implicit F: Contravariant[F])
      : F[(B, A)] = F.contramap(fa)(_._1)

  override final def toString = s"ContravariantCoyoneda(${liftee})(${fa})"
}

object ContravariantCoyoneda {
  /** Lift the `B` type member to a parameter.  It is usually more
    * convenient to use `Aux` than a structural type.
    */
  type Aux[F[_], A, B0] = ContravariantCoyoneda[F, A] {type B = B0}

  /** See `on` method. */
  final class On[F[_]] {
    @inline def apply[A, B0](liftee: A => B0)(implicit F: F[B0]): Aux[F, A, B0] =
      ContravariantCoyoneda.apply[F, A, B0](liftee)
  }

  /** Partial application of type parameters to `apply`.  It is often
    * more convenient to invoke `ContravariantCoyoneda.on[F]{x: X =>
    * ...}` then `ContravariantCoyoneda[...]{x: X => ...}`.
    */
  @inline def on[F[_]]: On[F] = new On[F]

  /** Create a new instance. */
  def apply[F[_], A, B0](_liftee: A => B0)(implicit F: F[B0]): Aux[F, A, B0] =
    new ContravariantCoyoneda[F, A]{
      type B = B0
      val liftee = _liftee
      val fa = F
    }

  /** Like `ContravariantCoyoneda(identity)(inst)`. */
  def lift[F[_], A](implicit F: F[A]): ContravariantCoyoneda[F, A] =
    apply(conforms[A])(F)
}
