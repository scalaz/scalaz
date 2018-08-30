package scalaz
package syntax

import Tags.Parallel

/** @see [[scalaz.syntax.ApplyOps]]`#|@|` */
private[scalaz] trait ApplicativeBuilder[M[_], A, B] {
  val a: M[A]
  val b: M[B]

  def apply[C](f: (A, B) => C)(implicit ap: Apply[M]): M[C] = ap.apply2(a, b)(f)

  def tupled(implicit ap: Apply[M]): M[(A, B)] = apply(Tuple2.apply)

  def ⊛[C](cc: M[C]) = new ApplicativeBuilder3[C] {
    val c = cc
  }

  def |@|[C](cc: M[C]): ApplicativeBuilder3[C] = ⊛(cc)

  sealed abstract class ApplicativeBuilder3[C] {
    val c: M[C]

    def apply[D](f: (A, B, C) => D)(implicit ap: Apply[M]): M[D] = ap.apply3(a, b, c)(f)

    def tupled(implicit ap: Apply[M]): M[(A, B, C)] = apply(Tuple3.apply)

    def ⊛[D](dd: M[D]) = new ApplicativeBuilder4[D] {
      val d = dd
    }

    def |@|[D](dd: M[D]): ApplicativeBuilder4[D] = ⊛(dd)

    def parApply[Z](f: (A, B, C) => Z)(implicit ap: Apply.Par[M]): M[Z] =
      Tag.unwrap(ap.apply3(
                   Tag[M[A], Parallel](a),
                   Tag[M[B], Parallel](b),
                   Tag[M[C], Parallel](c)
                 )(f))

    def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C)] =
      parApply(Tuple3.apply)

    sealed abstract class ApplicativeBuilder4[D] {
      val d: M[D]

      def apply[E](f: (A, B, C, D) => E)(implicit ap: Apply[M]): M[E] = ap.apply4(a, b, c, d)(f)

      def tupled(implicit ap: Apply[M]): M[(A, B, C, D)] = apply(Tuple4.apply)

      def ⊛[E](ee: M[E]) = new ApplicativeBuilder5[E] {
        val e = ee
      }

      def |@|[E](ee: M[E]): ApplicativeBuilder5[E] = ⊛(ee)

    def parApply[Z](f: (A, B, C, D) => Z)(implicit ap: Apply.Par[M]): M[Z] =
      Tag.unwrap(ap.apply4(
                   Tag[M[A], Parallel](a),
                   Tag[M[B], Parallel](b),
                   Tag[M[C], Parallel](c),
                   Tag[M[D], Parallel](d)
                 )(f))

    def parTupled(implicit ap: Apply.Par[M]): M[(A, B, C, D)] =
      parApply(Tuple4.apply)

      sealed abstract class ApplicativeBuilder5[E] {
        val e: M[E]

        def apply[F](f: (A, B, C, D, E) => F)(implicit ap: Apply[M]): M[F] = ap.apply5(a, b, c, d, e)(f)

        def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F] {
          val ff = f
        }

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

        sealed abstract class ApplicativeBuilder6[F] {
          val ff: M[F]

          def apply[G](f: (A, B, C, D, E, F) => G)(implicit ap: Apply[M]): M[G] = ap.apply6(a, b, c, d, e, ff)(f)

          def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ⊛[G](gg: M[G]) = new ApplicativeBuilder7[G] {
            val g = gg
          }

          def |@|[G](gg: M[G]): ApplicativeBuilder7[G] = ⊛(gg)

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

          sealed abstract class ApplicativeBuilder7[G] {
            val g: M[G]

            def apply[H](f: (A, B, C, D, E, F, G) => H)(implicit ap: Apply[M]): M[H] = ap.apply7(a, b, c, d, e, ff, g)(f)

            def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ⊛[H](hh: M[H]) = new ApplicativeBuilder8[H] {
              val h = hh
            }

            def |@|[H](hh: M[H]): ApplicativeBuilder8[H] = ⊛(hh)

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

            sealed abstract class ApplicativeBuilder8[H] {
              val h: M[H]

              def apply[I](f: (A, B, C, D, E, F, G, H) => I)(implicit ap: Apply[M]): M[I] = ap.apply8(a, b, c, d, e, ff, g, h)(f)

              def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

              def ⊛[I](ii: M[I]) = new ApplicativeBuilder9[I] {
                val i = ii
              }

              def |@|[I](ii: M[I]): ApplicativeBuilder9[I] = ⊛(ii)

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

              sealed abstract class ApplicativeBuilder9[I] {
                val i: M[I]

                def apply[J](f: (A, B, C, D, E, F, G, H, I) => J)(implicit ap: Apply[M]): M[J] = ap.apply9(a, b, c, d, e, ff, g, h, i)(f)

                def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

                def ⊛[J](jj: M[J]) = new ApplicativeBuilder10[J] {
                  val j = jj
                }

                def |@|[J](jj: M[J]): ApplicativeBuilder10[J] = ⊛(jj)

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

                sealed abstract class ApplicativeBuilder10[J] {
                  val j: M[J]

                  def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K)(implicit ap: Apply[M]): M[K] = ap.apply10(a, b, c, d, e, ff, g, h, i, j)(f)

                  def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J)] = apply(Tuple10.apply)

                  def ⊛[K](kk: M[K]) = new ApplicativeBuilder11[K] {
                    val k = kk
                  }

                  def |@|[K](kk: M[K]): ApplicativeBuilder11[K] = ⊛(kk)

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

                  sealed abstract class ApplicativeBuilder11[K] {
                    val k: M[K]

                    def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L)(implicit ap: Apply[M]): M[L] =
                      ap.apply11(a, b, c, d, e, ff, g, h, i, j, k)(f)

                    def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K)] = apply(Tuple11.apply)

                    def ⊛[L](ll: M[L]) = new ApplicativeBuilder12[L] {
                      val l = ll
                    }

                    def |@|[L](ll: M[L]): ApplicativeBuilder12[L] = ⊛(ll)

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

                    sealed abstract class ApplicativeBuilder12[L] {
                      val l: M[L]

                      def apply[MM](f: (A, B, C, D, E, F, G, H, I, J, K, L) => MM)(implicit ap: Apply[M]): M[MM] =
                        ap.apply12(a, b, c, d, e, ff, g, h, i, j, k, l)(f)

                      def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L)] = apply(Tuple12.apply)

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

// outside of ApplicativeBuilder for bincompat
object ApplicativeBuilder {
  implicit final class ApplicativeBuilderOps2[M[_], A, B](
      val self: ApplicativeBuilder[M, A, B]) extends AnyVal {
    def parApply[C](f: (A, B) => C)(implicit ap: Apply.Par[M]): M[C] =
      Tag.unwrap(ap.apply2(Tag[M[A], Parallel](self.a), Tag[M[B], Parallel](self.b))(f))

    def parTupled(implicit ap: Apply.Par[M]): M[(A, B)] =
      parApply(Tuple2.apply)
  }
}
