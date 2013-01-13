package scalaz

object Tag {
  /** `subst` specialized to `Id`.
    *
    * @todo According to Miles, @specialized doesn't help here. Maybe manually specialize.
    */
  @inline def apply[@specialized A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]

  /** Add a tag `T` to `A`. */
  def subst[A, F[_], T](fa: F[A]): F[A @@ T] = fa.asInstanceOf[F[A @@ T]]

  /** Remove the tag `T`, leaving `A`. */
  def unsubst[A, F[_], T](fa: F[A @@ T]): F[A] = fa.asInstanceOf[F[A]]
}
