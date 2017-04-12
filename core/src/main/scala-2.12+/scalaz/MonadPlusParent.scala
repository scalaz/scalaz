package scalaz

////
////
trait MonadPlusParent[F[_]] { self: MonadPlus[F] =>
  ////

  /** Generalized version of Haskell's `lefts` */
  def lefts[G[_, _], A, B](value: F[G[A, B]])(implicit G: Bifoldable[G]): F[A] =
    bind(value)(a => G.leftFoldable.foldMap(a)(point(_))(monoid[A]))

  /** Generalized version of Haskell's `rights` */
  def rights[G[_, _], A, B](value: F[G[A, B]])(implicit G: Bifoldable[G]): F[B] =
    bind(value)(b => G.rightFoldable.foldMap(b)(point(_))(monoid[B]))

  ////
}
