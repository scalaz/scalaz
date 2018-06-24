package scalaz
package syntax

import Tags.Parallel

/** @see [[scalaz.syntax.ApplyOps]]`#|@|` */
final class ApplicativeBuilder[M[_], A, B](a: M[A], b: M[B])(implicit ap: Apply[M]) {
  def apply[C](f: (A, B) => C): M[C] = ap.apply2(a, b)(f)

  def tupled: M[(A, B)] = apply(Tuple2.apply)

  def ⊛[C](c: M[C]) = new ApplicativeBuilder3[C](c)

  def |@|[C](c: M[C]): ApplicativeBuilder3[C] = ⊛(c)

  def parApply[C](f: (A, B) => C)(implicit ap: Apply.Par[M]): M[C] =
    Tag.unwrap(ap.apply2(Tag[M[A], Parallel](a), Tag[M[B], Parallel](b))(f))

  def parTupled(implicit ap: Apply.Par[M]): M[(A, B)] =
    parApply(Tuple2.apply)

  final class ApplicativeBuilder3[C](c: M[C]) {
    def apply[D](f: (A, B, C) => D): M[D] = ap.apply3(a, b, c)(f)

    def tupled: M[(A, B, C)] = apply(Tuple3.apply)

    def ⊛[D](d: M[D]) = new ApplicativeBuilder4[D](d)

    def |@|[D](d: M[D]): ApplicativeBuilder4[D] = ⊛(d)

    def parApply[Z](f: (A, B, C) => Z)(implicit ap: Apply.Par[M]): M[Z] =
      Tag.unwrap(ap.apply3(
                   Tag[M[A], Parallel](a),
                   Tag[M[B], Parallel](b),
                   Tag[M[C], Parallel](c)
                 )(f))

    def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C)] =
      parApply(Tuple3.apply)

    final class ApplicativeBuilder4[D](d: M[D]) {
      def apply[E](f: (A, B, C, D) => E): M[E] = ap.apply4(a, b, c, d)(f)

      def tupled: M[(A, B, C, D)] = apply(Tuple4.apply)

      def ⊛[E](e: M[E]) = new ApplicativeBuilder5[E](e)

      def |@|[E](e: M[E]): ApplicativeBuilder5[E] = ⊛(e)

      def parApply[Z](f: (A, B, C, D) => Z)(implicit ap: Apply.Par[M]): M[Z] =
        Tag.unwrap(ap.apply4(
                     Tag[M[A], Parallel](a),
                     Tag[M[B], Parallel](b),
                     Tag[M[C], Parallel](c),
                     Tag[M[D], Parallel](d)
                   )(f))

      def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D)] =
        parApply(Tuple4.apply)

      final class ApplicativeBuilder5[E](e: M[E]) {
        def apply[F](f: (A, B, C, D, E) => F): M[F] = ap.apply5(a, b, c, d, e)(f)

        def tupled: M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F](f)

        def |@|[F](f: M[F]): ApplicativeBuilder6[F] = ⊛(f)

        def parApply[Z](f: (A, B, C, D, E) => Z)(implicit ap: Apply.Par[M]): M[Z] =
          Tag.unwrap(ap.apply5(
                       Tag[M[A], Parallel](a),
                       Tag[M[B], Parallel](b),
                       Tag[M[C], Parallel](c),
                       Tag[M[D], Parallel](d),
                       Tag[M[E], Parallel](e)
                     )(f))

        def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E)] =
          parApply(Tuple5.apply)

        final class ApplicativeBuilder6[F](ff: M[F]) {
          def apply[G](f: (A, B, C, D, E, F) => G): M[G] = ap.apply6(a, b, c, d, e, ff)(f)

          def tupled: M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ⊛[G](g: M[G]) = new ApplicativeBuilder7[G](g)

          def |@|[G](g: M[G]): ApplicativeBuilder7[G] = ⊛(g)

          def parApply[Z](f: (A, B, C, D, E, F) => Z)(implicit ap: Apply.Par[M]): M[Z] =
            Tag.unwrap(ap.apply6(
                         Tag[M[A], Parallel](a),
                         Tag[M[B], Parallel](b),
                         Tag[M[C], Parallel](c),
                         Tag[M[D], Parallel](d),
                         Tag[M[E], Parallel](e),
                         Tag[M[F], Parallel](ff)
                       )(f))

          def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F)] =
            parApply(Tuple6.apply)

          final class ApplicativeBuilder7[G](g: M[G]) {
            def apply[H](f: (A, B, C, D, E, F, G) => H): M[H] = ap.apply7(a, b, c, d, e, ff, g)(f)

            def tupled: M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ⊛[H](h: M[H]) = new ApplicativeBuilder8[H](h)

            def |@|[H](h: M[H]): ApplicativeBuilder8[H] = ⊛(h)

            def parApply[Z](f: (A, B, C, D, E, F, G) => Z)(implicit ap: Apply.Par[M]): M[Z] =
              Tag.unwrap(ap.apply7(
                           Tag[M[A], Parallel](a),
                           Tag[M[B], Parallel](b),
                           Tag[M[C], Parallel](c),
                           Tag[M[D], Parallel](d),
                           Tag[M[E], Parallel](e),
                           Tag[M[F], Parallel](ff),
                           Tag[M[G], Parallel](g)
                         )(f))

            def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F, G)] =
              parApply(Tuple7.apply)

            final class ApplicativeBuilder8[H](h: M[H]) {
              def apply[I](f: (A, B, C, D, E, F, G, H) => I): M[I] = ap.apply8(a, b, c, d, e, ff, g, h)(f)

              def tupled: M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

              def ⊛[I](i: M[I]) = new ApplicativeBuilder9[I](i)

              def |@|[I](i: M[I]): ApplicativeBuilder9[I] = ⊛(i)

              def parApply[Z](f: (A, B, C, D, E, F, G, H) => Z)(implicit ap: Apply.Par[M]): M[Z] =
                Tag.unwrap(ap.apply8(
                             Tag[M[A], Parallel](a),
                             Tag[M[B], Parallel](b),
                             Tag[M[C], Parallel](c),
                             Tag[M[D], Parallel](d),
                             Tag[M[E], Parallel](e),
                             Tag[M[F], Parallel](ff),
                             Tag[M[G], Parallel](g),
                             Tag[M[H], Parallel](h)
                           )(f))

              def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F, G, H)] =
                parApply(Tuple8.apply)

              final class ApplicativeBuilder9[I](i: M[I]) {
                def apply[J](f: (A, B, C, D, E, F, G, H, I) => J): M[J] = ap.apply9(a, b, c, d, e, ff, g, h, i)(f)

                def tupled: M[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

                def ⊛[J](j: M[J]) = new ApplicativeBuilder10[J](j)

                def |@|[J](j: M[J]): ApplicativeBuilder10[J] = ⊛(j)

                def parApply[Z](f: (A, B, C, D, E, F, G, H, I) => Z)(implicit ap: Apply.Par[M]): M[Z] =
                  Tag.unwrap(ap.apply9(
                               Tag[M[A], Parallel](a),
                               Tag[M[B], Parallel](b),
                               Tag[M[C], Parallel](c),
                               Tag[M[D], Parallel](d),
                               Tag[M[E], Parallel](e),
                               Tag[M[F], Parallel](ff),
                               Tag[M[G], Parallel](g),
                               Tag[M[H], Parallel](h),
                               Tag[M[I], Parallel](i)
                             )(f))

                def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F, G, H, I)] =
                  parApply(Tuple9.apply)

                final class ApplicativeBuilder10[J](j: M[J]) {
                  def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K): M[K] = ap.apply10(a, b, c, d, e, ff, g, h, i, j)(f)

                  def tupled: M[(A, B, C, D, E, F, G, H, I, J)] = apply(Tuple10.apply)

                  def ⊛[K](k: M[K]) = new ApplicativeBuilder11[K](k)

                  def |@|[K](k: M[K]): ApplicativeBuilder11[K] = ⊛(k)

                  def parApply[Z](f: (A, B, C, D, E, F, G, H, I, J) => Z)(implicit ap: Apply.Par[M]): M[Z] =
                    Tag.unwrap(ap.apply10(
                                 Tag[M[A], Parallel](a),
                                 Tag[M[B], Parallel](b),
                                 Tag[M[C], Parallel](c),
                                 Tag[M[D], Parallel](d),
                                 Tag[M[E], Parallel](e),
                                 Tag[M[F], Parallel](ff),
                                 Tag[M[G], Parallel](g),
                                 Tag[M[H], Parallel](h),
                                 Tag[M[I], Parallel](i),
                                 Tag[M[J], Parallel](j)
                               )(f))

                  def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F, G, H, I, J)] =
                    parApply(Tuple10.apply)

                  final class ApplicativeBuilder11[K](k: M[K]) {
                    def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L): M[L] =
                      ap.apply11(a, b, c, d, e, ff, g, h, i, j, k)(f)

                    def tupled: M[(A, B, C, D, E, F, G, H, I, J, K)] = apply(Tuple11.apply)

                    def ⊛[L](l: M[L]) = new ApplicativeBuilder12[L](l)

                    def |@|[L](l: M[L]): ApplicativeBuilder12[L] = ⊛(l)

                    def parApply[Z](f: (A, B, C, D, E, F, G, H, I, J, K) => Z)(implicit ap: Apply.Par[M]): M[Z] =
                      Tag.unwrap(ap.apply11(
                                   Tag[M[A], Parallel](a),
                                   Tag[M[B], Parallel](b),
                                   Tag[M[C], Parallel](c),
                                   Tag[M[D], Parallel](d),
                                   Tag[M[E], Parallel](e),
                                   Tag[M[F], Parallel](ff),
                                   Tag[M[G], Parallel](g),
                                   Tag[M[H], Parallel](h),
                                   Tag[M[I], Parallel](i),
                                   Tag[M[J], Parallel](j),
                                   Tag[M[K], Parallel](k)
                                 )(f))

                    def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F, G, H, I, J, K)] =
                      parApply(Tuple11.apply)

                    final class ApplicativeBuilder12[L](l: M[L]) {
                      def apply[MM](f: (A, B, C, D, E, F, G, H, I, J, K, L) => MM): M[MM] =
                        ap.apply12(a, b, c, d, e, ff, g, h, i, j, k, l)(f)

                      def tupled: M[(A, B, C, D, E, F, G, H, I, J, K, L)] = apply(Tuple12.apply)

                      def parApply[Z](f: (A, B, C, D, E, F, G, H, I, J, K, L) => Z)(implicit ap: Apply.Par[M]): M[Z] =
                        Tag.unwrap(ap.apply12(
                                     Tag[M[A], Parallel](a),
                                     Tag[M[B], Parallel](b),
                                     Tag[M[C], Parallel](c),
                                     Tag[M[D], Parallel](d),
                                     Tag[M[E], Parallel](e),
                                     Tag[M[F], Parallel](ff),
                                     Tag[M[G], Parallel](g),
                                     Tag[M[H], Parallel](h),
                                     Tag[M[I], Parallel](i),
                                     Tag[M[J], Parallel](j),
                                     Tag[M[K], Parallel](k),
                                     Tag[M[L], Parallel](l)
                                   )(f))

                      def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L)] =
                        parApply(Tuple12.apply)

                    }

                  }

                }

              }

            }

          }

        }

      }

    }

  }

}
