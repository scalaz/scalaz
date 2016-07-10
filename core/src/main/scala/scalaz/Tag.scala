package scalaz

import Id._

object Tag {
  /** `subst` specialized to `Id`.
    *
    * @todo According to Miles, @specialized doesn't help here. Maybe manually specialize.
    */
  @inline def apply[@specialized A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]

  /** `unsubst` specialized to `Id`. */
  @inline def unwrap[@specialized A, T](a: A @@ T): A = unsubst[A, Id, T](a)

  /** Add a tag `T` to `A`.
    *
    * NB: It is unsafe to `subst` or `unsubst` a tag in an `F` that is
    * sensitive to the `A` type within.  For example, if `F` is a
    * GADT, rather than a normal ADT, it is probably unsafe.  For
    * "normal" types like `List` and function types, it is safe.  More
    * broadly, if it is possible to write a ''legal''
    * [[scalaz.InvariantFunctor]] over the parameter, `subst` of that
    * parameter is safe.
    *
    * We do not have a
    * <a href="https://ghc.haskell.org/trac/ghc/wiki/Roles">type role</a>
    * system in Scala with which to declare the exact situations under
    * which `subst` is safe.  If we did, we would declare that `subst`
    * is safe if and only if the parameter has "representational" or
    * "phantom" role.
    */
  def subst[A, F[_], T](fa: F[A]): F[A @@ T] = fa.asInstanceOf[F[A @@ T]]

  /** Add a tag `T` to `G[_]` */
  def subst1[G[_], F[_[_]], T](fa: F[G]): F[λ[α => G[α] @@ T]] = fa.asInstanceOf[F[λ[α => G[α] @@ T]]]

  /** Remove the tag `T`, leaving `A`. */
  def unsubst[A, F[_], T](fa: F[A @@ T]): F[A] = fa.asInstanceOf[F[A]]

  /** @see `Tag.of` */
  final class TagOf[T] private[Tag]()
      extends (Id.Id ~> (? @@ T)) {
    /** Like `Tag.apply`, but specify only the `T`. */
    def apply[A](a: A): A @@ T = Tag.apply(a)

    /** Like `Tag.unwrap`, but specify only the `T`. */
    def unwrap[A](a: A @@ T): A = Tag.unwrap(a)

    /** Like `Tag.subst`, but specify only the `T`. */
    def subst[F[_], A](fa: F[A]): F[A @@ T] = Tag.subst(fa)

    /** Like `Tag.subst1`, but specify only the `T`. */
    def subst1[F[_[_]], G[_]](fa: F[G]): F[λ[α => G[α] @@ T]] = Tag.subst1[G, F, T](fa)

    /** Tag `fa`'s return type.  Allows inference of `A` to "flow through" from
      * the enclosing context.
      */
    def onF[A, B](fa: A => B): A => (B @@ T) =
      subst[A => ?, B](fa)

    /** One variant of `subst` with different inference. */
    def onCov[FA](fa: FA)(implicit U: Unapply[Functor, FA]): U.M[U.A @@ T] =
      subst(U(fa))

    /** One variant of `subst` with different inference. */
    def onContra[FA](fa: FA)(implicit U: Unapply[Contravariant, FA]): U.M[U.A @@ T] =
      subst(U(fa))

    /** Like `Tag.unsubst`, but specify only the `T`. */
    def unsubst[F[_], A](fa: F[A @@ T]): F[A] = Tag.unsubst(fa)

    /** Pattern match on a tagged value */
    def unapply[A](a: A @@ T): Option[A] = Some(unwrap(a))
  }

  /** Variants of `apply`, `subst`, and `unsubst` that require
    * specifying the tag type but are more likely to infer the other
    * type parameters.
    */
  def of[T]: TagOf[T] = new TagOf[T]
}
