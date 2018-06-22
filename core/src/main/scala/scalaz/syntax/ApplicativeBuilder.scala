package scalaz
package syntax

import Tags.Parallel

/** @see [[scalaz.syntax.ApplyOps]]`#|@|` */
private[scalaz] trait ApplicativeBuilder[M[_], A, B] {
  val a: M[A]
  val b: M[B]

  def apply[C](f: (A, B) => C)(implicit ap: Apply[M]): M[C] = ap.apply2(a, b)(f)

  def tupled(implicit ap: Apply[M]): M[(A, B)] = apply(Tuple2.apply)

  // bincompat :-(
  //def parApply[C](f: (A, B) => C)(implicit ap: Apply[λ[α => M[α] @@ Parallel]]): M[C] =
  //  Parallel.unwrap(ap.apply2(Parallel(a), Parallel(b))(f))
  //def parTupled(implicit ap: Apply[λ[α => M[α] @@ Parallel]]): M[(A, B)] =
  //  parApply(Tuple2.apply)

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

    sealed abstract class ApplicativeBuilder4[D] {
      val d: M[D]

      def apply[E](f: (A, B, C, D) => E)(implicit ap: Apply[M]): M[E] = ap.apply4(a, b, c, d)(f)

      def tupled(implicit ap: Apply[M]): M[(A, B, C, D)] = apply(Tuple4.apply)

      def ⊛[E](ee: M[E]) = new ApplicativeBuilder5[E] {
        val e = ee
      }

      def |@|[E](ee: M[E]): ApplicativeBuilder5[E] = ⊛(ee)

      sealed abstract class ApplicativeBuilder5[E] {
        val e: M[E]

        def apply[F](f: (A, B, C, D, E) => F)(implicit ap: Apply[M]): M[F] = ap.apply5(a, b, c, d, e)(f)

        def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F] {
          val ff = f
        }

        def |@|[F](f: M[F]): ApplicativeBuilder6[F] = ⊛(f)

        sealed abstract class ApplicativeBuilder6[F] {
          val ff: M[F]

          def apply[G](f: (A, B, C, D, E, F) => G)(implicit ap: Apply[M]): M[G] = ap.apply6(a, b, c, d, e, ff)(f)

          def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ⊛[G](gg: M[G]) = new ApplicativeBuilder7[G] {
            val g = gg
          }

          def |@|[G](gg: M[G]): ApplicativeBuilder7[G] = ⊛(gg)

          sealed abstract class ApplicativeBuilder7[G] {
            val g: M[G]

            def apply[H](f: (A, B, C, D, E, F, G) => H)(implicit ap: Apply[M]): M[H] = ap.apply7(a, b, c, d, e, ff, g)(f)

            def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ⊛[H](hh: M[H]) = new ApplicativeBuilder8[H] {
              val h = hh
            }

            def |@|[H](hh: M[H]): ApplicativeBuilder8[H] = ⊛(hh)

            sealed abstract class ApplicativeBuilder8[H] {
              val h: M[H]

              def apply[I](f: (A, B, C, D, E, F, G, H) => I)(implicit ap: Apply[M]): M[I] = ap.apply8(a, b, c, d, e, ff, g, h)(f)

              def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

              def ⊛[I](ii: M[I]) = new ApplicativeBuilder9[I] {
                val i = ii
              }

              def |@|[I](ii: M[I]): ApplicativeBuilder9[I] = ⊛(ii)

              sealed abstract class ApplicativeBuilder9[I] {
                val i: M[I]

                def apply[J](f: (A, B, C, D, E, F, G, H, I) => J)(implicit ap: Apply[M]): M[J] = ap.apply9(a, b, c, d, e, ff, g, h, i)(f)

                def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

                def ⊛[J](jj: M[J]) = new ApplicativeBuilder10[J] {
                  val j = jj
                }

                def |@|[J](jj: M[J]): ApplicativeBuilder10[J] = ⊛(jj)

                sealed abstract class ApplicativeBuilder10[J] {
                  val j: M[J]

                  def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K)(implicit ap: Apply[M]): M[K] = ap.apply10(a, b, c, d, e, ff, g, h, i, j)(f)

                  def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J)] = apply(Tuple10.apply)

                  def ⊛[K](kk: M[K]) = new ApplicativeBuilder11[K] {
                    val k = kk
                  }

                  def |@|[K](kk: M[K]): ApplicativeBuilder11[K] = ⊛(kk)

                  sealed abstract class ApplicativeBuilder11[K] {
                    val k: M[K]

                    def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L)(implicit ap: Apply[M]): M[L] =
                      ap.apply11(a, b, c, d, e, ff, g, h, i, j, k)(f)

                    def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K)] = apply(Tuple11.apply)

                    def ⊛[L](ll: M[L]) = new ApplicativeBuilder12[L] {
                      val l = ll
                    }

                    def |@|[L](ll: M[L]): ApplicativeBuilder12[L] = ⊛(ll)

                    sealed abstract class ApplicativeBuilder12[L] {
                      val l: M[L]

                      def apply[MM](f: (A, B, C, D, E, F, G, H, I, J, K, L) => MM)(implicit ap: Apply[M]): M[MM] =
                        ap.apply12(a, b, c, d, e, ff, g, h, i, j, k, l)(f)

                      def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L)] = apply(Tuple12.apply)
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
