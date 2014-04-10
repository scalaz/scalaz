package scalaz

object Tag {
  /** `subst` specialized to `Id`.
    *
    * @todo According to Miles, @specialized doesn't help here. Maybe manually specialize.
    */
  @inline def apply[@specialized A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]

  /** Adds a tag which can not accidentally be lost. */
  def lossless[A, T](a: A): A @@@ T = a.asInstanceOf[A @@@ T]

  /** Removes a "lossless" tag. */
  def unwrap[A, T](a: A @@@ T): A = a.asInstanceOf[A]

  /** Add a tag `T` to `A`. */
  def subst[A, F[_], T](fa: F[A]): F[A @@ T] = fa.asInstanceOf[F[A @@ T]]

  /** Remove the tag `T`, leaving `A`. */
  def unsubst[A, F[_], T](fa: F[A @@ T]): F[A] = fa.asInstanceOf[F[A]]

  /** @see `Tag.of` */
  final class TagOf[T] private[Tag]()
      extends (Id.Id ~> ({type λ[α] = α @@ T})#λ) {
    /** Like `Tag.apply`, but specify only the `T`. */
    def apply[A](a: A): A @@ T = Tag.apply(a)

    /** Like `Tag.lossless`, but specify only the `T`. */
    def lossless[A](a: A): A @@@ T = Tag.lossless(a)

    /** Like `Tag.unwrap`, but specify only the `T`. */
    def unwrap[A](a: A @@@ T): A = Tag.unwrap(a)

    /** Like `Tag.subst`, but specify only the `T`. */
    def subst[F[_], A](fa: F[A]): F[A @@ T] = Tag.subst(fa)

    /** Tag `fa`'s return type.  Allows inference of `A` to "flow through" from
      * the enclosing context.
      */
    def onF[A, B](fa: A => B): A => (B @@ T) =
      subst[({type λ[α] = A => α})#λ, B](fa)

    /** One variant of `subst` with different inference. */
    def onCov[FA](fa: FA)(implicit U: Unapply[Functor, FA]): U.M[U.A @@ T] =
      subst(U(fa))

    /** One variant of `subst` with different inference. */
    def onContra[FA](fa: FA)(implicit U: Unapply[Contravariant, FA]): U.M[U.A @@ T] =
      subst(U(fa))

    /** Like `Tag.unsubst`, but specify only the `T`. */
    def unsubst[F[_], A](fa: F[A @@ T]): F[A] = Tag.unsubst(fa)
  }

  /** Variants of `apply`, `subst`, and `unsubst` that require
    * specifying the tag type but are more likely to infer the other
    * type parameters.
    */
  def of[T]: TagOf[T] = new TagOf[T]

  /** Isomorphisms between the two types of tags and values. */
  import Isomorphism.<=>

  def losslessi[T, A] = new (A <=> (A @@@ T)) {
    def from: A @@@ T => A = _.asInstanceOf[A]
    def to: A => A @@@ T = _.asInstanceOf[A @@@ T]
  }

  def tagsi[T, A] = new ((A @@ T) <=> (A @@@ T)) {
    def from: A @@@ T => A @@ T = _.asInstanceOf[A @@ T]
    def to: A @@ T => A @@@ T = _.asInstanceOf[A @@@ T]
  }
}
