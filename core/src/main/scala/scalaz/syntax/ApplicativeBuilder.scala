package scalaz
package syntax

/** @see [[scalaz.syntax.ApplyV]]`#|@|` */
private[scalaz] trait ApplicativeBuilder[M[_], A, B] {
  val a: M[A]
  val b: M[B]

  def apply[C](f: (A, B) => C)(implicit ap: Apply[M]): M[C] = ap.map2(a, b)(f)

  def tupled(implicit ap: Apply[M]): M[(A, B)] = apply(Tuple2.apply)

  def ⊛[C](cc: M[C]) = new ApplicativeBuilder3[C] {
    val c = cc
  }

  def |@|[C](cc: M[C]) = ⊛(cc)

  sealed trait ApplicativeBuilder3[C] {
    val c: M[C]

    def apply[D](f: (A, B, C) => D)(implicit ap: Apply[M]): M[D] = ap.map3(a, b, c)(f)

    def tupled(implicit ap: Apply[M]): M[(A, B, C)] = apply(Tuple3.apply)

    def ⊛[D](dd: M[D]) = new ApplicativeBuilder4[D] {
      val d = dd
    }

    def |@|[D](dd: M[D]) = ⊛(dd)

    sealed trait ApplicativeBuilder4[D] {
      val d: M[D]

      def apply[E](f: (A, B, C, D) => E)(implicit ap: Apply[M]): M[E] = ap.map4(a, b, c, d)(f)

      def tupled(implicit ap: Apply[M]): M[(A, B, C, D)] = apply(Tuple4.apply)

      def ⊛[E](ee: M[E]) = new ApplicativeBuilder5[E] {
        val e = ee
      }

      def |@|[E](ee: M[E]) = ⊛(ee)

      sealed trait ApplicativeBuilder5[E] {
        val e: M[E]

        def apply[F](f: (A, B, C, D, E) => F)(implicit ap: Apply[M]): M[F] = ap.map5(a, b, c, d, e)(f)

        def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F] {
          val ff = f
        }

        def |@|[F](f: M[F]) = ⊛(f)

        sealed trait ApplicativeBuilder6[F] {
          val ff: M[F]

          def apply[G](f: (A, B, C, D, E, F) => G)(implicit ap: Apply[M]): M[G] = ap.map6(a, b, c, d, e, ff)(f)

          def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ⊛[G](gg: M[G]) = new ApplicativeBuilder7[G] {
            val g = gg
          }

          def |@|[G](gg: M[G]) = ⊛(gg)

          sealed trait ApplicativeBuilder7[G] {
            val g: M[G]

            def apply[H](f: (A, B, C, D, E, F, G) => H)(implicit ap: Apply[M]): M[H] = ap.map7(a, b, c, d, e, ff, g)(f)

            def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ⊛[H](hh: M[H]) = new ApplicativeBuilder8[H] {
              val h = hh
            }

            def |@|[H](hh: M[H]) = ⊛(hh)

            sealed trait ApplicativeBuilder8[H] {
              val h: M[H]

              def apply[I](f: (A, B, C, D, E, F, G, H) => I)(implicit ap: Apply[M]): M[I] = ap.map8(a, b, c, d, e, ff, g, h)(f)

              def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

              def ⊛[I](ii: M[I]) = new ApplicativeBuilder9[I] {
                val i = ii
              }

              def |@|[I](ii: M[I]) = ⊛(ii)

              sealed trait ApplicativeBuilder9[I] {
                val i: M[I]

                def apply[J](f: (A, B, C, D, E, F, G, H, I) => J)(implicit ap: Apply[M]): M[J] = ap.map9(a, b, c, d, e, ff, g, h, i)(f)

                def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

                def ⊛[J](jj: M[J]) = new ApplicativeBuilder10[J] {
                  val j = jj
                }

                def |@|[J](jj: M[J]) = ⊛(jj)

                sealed trait ApplicativeBuilder10[J] {
                  val j: M[J]

                  def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K)(implicit ap: Apply[M]): M[K] = ap.map10(a, b, c, d, e, ff, g, h, i, j)(f)

                  def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J)] = apply(Tuple10.apply)

                  def ⊛[K](kk: M[K]) = new ApplicativeBuilder11[K] {
                    val k = kk
                  }

                  def |@|[K](kk: M[K]) = ⊛(kk)

                  sealed trait ApplicativeBuilder11[K] {
                    val k: M[K]

                    def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L)(implicit ap: Apply[M]): M[L] =
                      ap.map11(a, b, c, d, e, ff, g, h, i, j, k)(f)

                    def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K)] = apply(Tuple11.apply)

                    def ⊛[L](ll: M[L]) = new ApplicativeBuilder12[L] {
                      val l = ll
                    }

                    def |@|[L](ll: M[L]) = ⊛(ll)

                    sealed trait ApplicativeBuilder12[L] {
                      val l: M[L]

                      def apply[MM](f: (A, B, C, D, E, F, G, H, I, J, K, L) => MM)(implicit ap: Apply[M]): M[MM] =
                        ap.map12(a, b, c, d, e, ff, g, h, i, j, k, l)(f)

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
