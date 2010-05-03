package scalaz

/**
 * Not intended for direct use. Instead use  { @link scalaz.MA # ⊛ }
 */
final class ApplicativeBuilder[M[_], A, B](a: M[A], b: M[B]) {
  def apply[C](f: (A, B) => C)(implicit t: Functor[M], ap: Apply[M]): M[C] = ap(t.fmap(a, f.curried), b)

  def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B)] = apply(Tuple2.apply)

  def ⊛[C](c: M[C]) = new ApplicativeBuilder3[C](c)

  def |@|[C](c: M[C]) = new ApplicativeBuilder3[C](c)

  final class ApplicativeBuilder3[C](c: M[C]) {
    def apply[D](f: (A, B, C) => D)(implicit t: Functor[M], ap: Apply[M]): M[D] = ap(ap(t.fmap(a, f.curried), b), c)

    def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B, C)] = apply(Tuple3.apply)

    def ⊛[D](d: M[D]) = new ApplicativeBuilder4[D](d)

    def |@|[D](d: M[D]) = new ApplicativeBuilder4[D](d)

    final class ApplicativeBuilder4[D](d: M[D]) {
      def apply[E](f: (A, B, C, D) => E)(implicit t: Functor[M], ap: Apply[M]): M[E] = ap(ap(ap(t.fmap(a, f.curried), b), c), d)

      def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B, C, D)] = apply(Tuple4.apply)

      def ⊛[E](e: M[E]) = new ApplicativeBuilder5[E](e)

      def |@|[E](e: M[E]) = new ApplicativeBuilder5[E](e)

      final class ApplicativeBuilder5[E](e: M[E]) {
        def apply[F](f: (A, B, C, D, E) => F)(implicit t: Functor[M], ap: Apply[M]): M[F] = ap(ap(ap(ap(t.fmap(a, f.curried), b), c), d), e)

        def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F](f)

        def |@|[F](f: M[F]) = new ApplicativeBuilder6[F](f)

        final class ApplicativeBuilder6[F](ff: M[F]) {
          def apply[G](f: (A, B, C, D, E, F) => G)(implicit t: Functor[M], ap: Apply[M]): M[G] = ap(ap(ap(ap(ap(t.fmap(a, f.curried), b), c), d), e), ff)

          def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ⊛[G](g: M[G]) = new ApplicativeBuilder7[G](g)

          def |@|[G](g: M[G]) = new ApplicativeBuilder7[G](g)

          class ApplicativeBuilder7[G](g: M[G]) {
            def apply[H](f: (A, B, C, D, E, F, G) => H)(implicit t: Functor[M], ap: Apply[M]): M[H] = ap(ap(ap(ap(ap(ap(t.fmap(a, f.curried), b), c), d), e), ff), g)

            def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ⊛[H](h: M[H]) = new ApplicativeBuilder8[H](h)

            def |@|[H](h: M[H]) = new ApplicativeBuilder8[H](h)

            final class ApplicativeBuilder8[H](h: M[H]) {
              def apply[I](f: (A, B, C, D, E, F, G, H) => I)(implicit t: Functor[M], ap: Apply[M]): M[I] = ap(ap(ap(ap(ap(ap(ap(t.fmap(a, f.curried), b), c), d), e), ff), g), h)

              def tupled(implicit t: Functor[M], ap: Apply[M]): M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)
            }
          }
        }
      }
    }
  }
}
