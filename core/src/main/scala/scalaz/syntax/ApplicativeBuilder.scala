package scalaz
package syntax

/** @see [[scalaz.syntax.ApplyV]]`#|@|` */
private[scalaz] trait ApplicativeBuilder[M[_], A, B] {
  val a: M[A]
  val b: M[B]

  def apply[C](f: (A, B) => C)(implicit ap: Apply[M]): M[C] = ap(a, b)(f)

  def tupled(implicit ap: Apply[M]): M[(A, B)] = apply(Tuple2.apply)

  def ⊛[C](cc: M[C]) = new ApplicativeBuilder3[C] {
    val c = cc
  }

  def |@|[C](cc: M[C]) = ⊛(cc)

  sealed trait ApplicativeBuilder3[C] {
    val c: M[C]

    def apply[D](f: (A, B, C) => D)(implicit ap: Apply[M]): M[D] = ap(a, b, c)(f)

    def tupled(implicit ap: Apply[M]): M[(A, B, C)] = apply(Tuple3.apply)

    def ⊛[D](dd: M[D]) = new ApplicativeBuilder4[D] {
      val d = dd
    }

    def |@|[D](dd: M[D]) = ⊛(dd)

    sealed trait ApplicativeBuilder4[D] {
      val d: M[D]

      def apply[E](f: (A, B, C, D) => E)(implicit ap: Apply[M]): M[E] = ap(a, b, c, d)(f)

      def tupled(implicit ap: Apply[M]): M[(A, B, C, D)] = apply(Tuple4.apply)

      def ⊛[E](ee: M[E]) = new ApplicativeBuilder5[E] {
        val e = ee
      }

      def |@|[E](ee: M[E]) = ⊛(ee)

      sealed trait ApplicativeBuilder5[E] {
        val e: M[E]

        def apply[F](f: (A, B, C, D, E) => F)(implicit ap: Apply[M]): M[F] = ap(a, b, c, d, e)(f)

        def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E)] = apply(Tuple5.apply)

        def ⊛[F](f: M[F]) = new ApplicativeBuilder6[F] {
          val ff = f
        }

        def |@|[F](f: M[F]) = ⊛(f)

        sealed trait ApplicativeBuilder6[F] {
          val ff: M[F]

          def apply[G](f: (A, B, C, D, E, F) => G)(implicit ap: Apply[M]): M[G] = ap(a, b, c, d, e, ff)(f)

          def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F)] = apply(Tuple6.apply)

          def ⊛[G](gg: M[G]) = new ApplicativeBuilder7[G] {
            val g = gg
          }

          def |@|[G](gg: M[G]) = ⊛(gg)

          sealed trait ApplicativeBuilder7[G] {
            val g: M[G]

            def apply[H](f: (A, B, C, D, E, F, G) => H)(implicit ap: Apply[M]): M[H] = ap(a, b, c, d, e, ff, g)(f)

            def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G)] = apply(Tuple7.apply)

            def ⊛[H](hh: M[H]) = new ApplicativeBuilder8[H] {
              val h = hh
            }

            def |@|[H](hh: M[H]) = ⊛(hh)

            sealed trait ApplicativeBuilder8[H] {
              val h: M[H]

              def apply[I](f: (A, B, C, D, E, F, G, H) => I)(implicit ap: Apply[M]): M[I] = ap(a, b, c, d, e, ff, g, h)(f)

              def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H)] = apply(Tuple8.apply)

              def ⊛[I](ii: M[I]) = new ApplicativeBuilder9[I] {
                val i = ii
              }

              def |@|[I](ii: M[I]) = ⊛(ii)

              sealed trait ApplicativeBuilder9[I] {
                val i: M[I]

                def apply[J](f: (A, B, C, D, E, F, G, H, I) => J)(implicit ap: Apply[M]): M[J] = ap(a, b, c, d, e, ff, g, h, i)(f)

                def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I)] = apply(Tuple9.apply)

                def ⊛[J](jj: M[J]) = new ApplicativeBuilder10[J] {
                  val j = jj
                }

                def |@|[J](jj: M[J]) = ⊛(jj)

                sealed trait ApplicativeBuilder10[J] {
                  val j: M[J]

                  def apply[K](f: (A, B, C, D, E, F, G, H, I, J) => K)(implicit ap: Apply[M]): M[K] = ap(a, b, c, d, e, ff, g, h, i, j)(f)

                  def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J)] = apply(Tuple10.apply)

                  def ⊛[K](kk: M[K]) = new ApplicativeBuilder11[K] {
                    val k = kk
                  }

                  def |@|[K](kk: M[K]) = ⊛(kk)

                  sealed trait ApplicativeBuilder11[K] {
                    val k: M[K]

                    def apply[L](f: (A, B, C, D, E, F, G, H, I, J, K) => L)(implicit ap: Apply[M]): M[L] =
                      ap(a, b, c, d, e, ff, g, h, i, j, k)(f)

                    def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K)] = apply(Tuple11.apply)

                    def ⊛[L](ll: M[L]) = new ApplicativeBuilder12[L] {
                      val l = ll
                    }

                    def |@|[L](ll: M[L]) = ⊛(ll)

                    sealed trait ApplicativeBuilder12[L] {
                      val l: M[L]

                      def apply[MM](f: (A, B, C, D, E, F, G, H, I, J, K, L) => MM)(implicit ap: Apply[M]): M[MM] =
                        ap(a, b, c, d, e, ff, g, h, i, j, k, l)(f)

                      def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L)] = apply(Tuple12.apply)

                      def ⊛[MM](mm: M[MM]) = new ApplicativeBuilder13[MM] {
                        val m = mm
                      }

                      def |@|[MM](mm: M[MM]) = ⊛(mm)

                      sealed trait ApplicativeBuilder13[MM] {
                        val m: M[MM]

                        def apply[N](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM) => N)(implicit ap: Apply[M]): M[N] =
                          ap(a, b, c, d, e, ff, g, h, i, j, k, l, m)(f)

                        def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM)] = apply(Tuple13.apply)

                        def ⊛[N](nn: M[N]) = new ApplicativeBuilder14[N] {
                          val n = nn
                        }

                        def |@|[N](nn: M[N]) = ⊛(nn)

                        sealed trait ApplicativeBuilder14[N] {
                          val n: M[N]

                          def apply[O](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N) => O)(implicit ap: Apply[M]): M[O] =
                            ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n)(f)

                          def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N)] = apply(Tuple14.apply)

                          def ⊛[O](oo: M[O]) = new ApplicativeBuilder15[O] {
                            val o = oo
                          }

                          def |@|[O](oo: M[O]) = ⊛(oo)

                          sealed trait ApplicativeBuilder15[O] {
                            val o: M[O]

                            def apply[P](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O) => P)(implicit ap: Apply[M]): M[P] =
                              ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o)(f)

                            def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O)] = apply(Tuple15.apply)

                            def ⊛[P](pp: M[P]) = new ApplicativeBuilder16[P] {
                              val p = pp
                            }

                            def |@|[P](pp: M[P]) = ⊛(pp)

                            sealed trait ApplicativeBuilder16[P] {
                              val p: M[P]

                              def apply[Q](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P) => Q)(implicit ap: Apply[M]): M[Q] =
                                ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p)(f)

                              def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P)] = apply(Tuple16.apply)

                              def ⊛[Q](qq: M[Q]) = new ApplicativeBuilder17[Q] {
                                val q = qq
                              }

                              def |@|[Q](qq: M[Q]) = ⊛(qq)

                              sealed trait ApplicativeBuilder17[Q] {
                                val q: M[Q]

                                def apply[R](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q) => R)(implicit ap: Apply[M]): M[R] =
                                  ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q)(f)

                                def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q)] = apply(Tuple17.apply)

                                def ⊛[R](rr: M[R]) = new ApplicativeBuilder18[R] {
                                  val r = rr
                                }

                                def |@|[R](rr: M[R]) = ⊛(rr)

                                sealed trait ApplicativeBuilder18[R] {
                                  val r: M[R]

                                  def apply[S](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R) => S)(implicit ap: Apply[M]): M[S] =
                                    ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r)(f)

                                  def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R)] = apply(Tuple18.apply)

                                  def ⊛[S](ss: M[S]) = new ApplicativeBuilder19[S] {
                                    val s = ss
                                  }

                                  def |@|[S](ss: M[S]) = ⊛(ss)

                                  sealed trait ApplicativeBuilder19[S] {
                                    val s: M[S]

                                    def apply[T](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S) => T)(implicit ap: Apply[M]): M[T] =
                                      ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s)(f)

                                    def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S)] = apply(Tuple19.apply)

                                    def ⊛[T](tt: M[T]) = new ApplicativeBuilder20[T] {
                                      val t = tt
                                    }

                                    def |@|[T](tt: M[T]) = ⊛(tt)

                                    sealed trait ApplicativeBuilder20[T] {
                                      val t: M[T]

                                      def apply[U](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S, T) => U)(implicit ap: Apply[M]): M[U] =
                                        ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(f)

                                      def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S, T)] = apply(Tuple20.apply)

                                      def ⊛[U](uu: M[U]) = new ApplicativeBuilder21[U] {
                                        val u = uu
                                      }

                                      def |@|[U](uu: M[U]) = ⊛(uu)

                                      sealed trait ApplicativeBuilder21[U] {
                                        val u: M[U]

                                        def apply[V](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S, T, U) => V)(implicit ap: Apply[M]): M[V] =
                                          ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(f)

                                        def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S, T, U)] = apply(Tuple21.apply)

                                        def ⊛[V](vv: M[V]) = new ApplicativeBuilder22[V] {
                                          val v = vv
                                        }

                                        def |@|[V](vv: M[V]) = ⊛(vv)

                                        sealed trait ApplicativeBuilder22[V] {
                                          val v: M[V]

                                          def apply[W](f: (A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S, T, U, V) => W)(implicit ap: Apply[M]): M[W] =
                                            ap(a, b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)(f)

                                          def tupled(implicit ap: Apply[M]): M[(A, B, C, D, E, F, G, H, I, J, K, L, MM, N, O, P, Q, R, S, T, U, V)] = apply(Tuple22.apply)

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
