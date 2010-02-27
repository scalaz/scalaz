package scalaz

/**
 * Not intended for direct use. Instead use {@link scalaz.MA#⊛}
 */
final class ApplicativeBuilder[M[_], A, B](a: M[A], b: M[B])(implicit t: Functor[M], ap: Apply[M]) {
  import t.fmap

  def apply[C](f: (A, B) => C): M[C] = ap(fmap(a, f.curried), b)

  def ⊛[C](c: M[C]) = new ApplicativeBuilder3[C](c)

  final class ApplicativeBuilder3[C](c: M[C]) {
    def apply[D](f: (A, B, C) => D): M[D] = ap(ap(fmap(a, f.curried), b), c)

    def ⊛[D](d: M[D]) = new ApplicativeBuilder4[D](d)

    final class ApplicativeBuilder4[D](d: M[D]) {
      def apply[E](f: (A, B, C, D) => E): M[E] = ap(ap(ap(fmap(a, f.curried), b), c), d)

      def ⊛[E](e: M[E]) = new ApplicativeBuilder5[E](e)

      final class ApplicativeBuilder5[E](e: M[E]) {
        def apply[F](f: (A, B, C, D, E) => F): M[F] = ap(ap(ap(ap(fmap(a, f.curried), b), c), d), e)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F](f)

        final class ApplicativeBuilder6[F](ff: M[F]) {
          def apply[G](f: (A, B, C, D, E, F) => G): M[G] = ap(ap(ap(ap(ap(fmap(a, f.curried), b), c), d), e), ff)

          def ⊛[G](g: M[G]) = new ApplicativeBuilder7[G](g)

          class ApplicativeBuilder7[G](g: M[G]) {
            def apply[H](f: (A, B, C, D, E, F, G) => H): M[H] = ap(ap(ap(ap(ap(ap(fmap(a, f.curried), b), c), d), e), ff), g)

            def ⊛[H](h: M[H]) = new ApplicativeBuilder8[H](h)

            final class ApplicativeBuilder8[H](h: M[H]) {
              def apply[I](f: (A, B, C, D, E, F, G, H) => I): M[I] = ap(ap(ap(ap(ap(ap(ap(fmap(a, f.curried), b), c), d), e), ff), g), h)
            }
          }
        }
      }
    }
  }
}
