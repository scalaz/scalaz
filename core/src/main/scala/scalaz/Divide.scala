package scalaz

////
/** `Divide` is the contravariant analogue of `scalaz.Apply`
 *
 * @see [[https://github.com/ekmett/contravariant/issues/18]]
 */
////
trait Divide[F[_]] extends Contravariant[F] { self =>
  ////
  def divide[A, B, C](fa: F[A], fb: F[B])(f: C => (A, B)): F[C]

  /** alias for `contramap` */
  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    contramap(a1)(f)

  /** alias for `divide` */
  final def divide2[A1, A2, Z](a1: F[A1], a2: F[A2])(f: Z => (A1, A2)): F[Z] =
    divide(a1, a2)(f)

  def divide3[A1, A2, A3, Z](a1: F[A1], a2: F[A2], a3: F[A3])(f: Z => (A1, A2, A3)): F[Z] =
    divide2(tuple2(a1, a2), a3){ z =>
      val t = f(z)
      ((t._1, t._2), t._3)
    }

  def divide4[A1, A2, A3, A4, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4])(f: Z => (A1, A2, A3, A4)): F[Z] =
    divide2(tuple2(a1, a2), tuple2(a3, a4)){ z =>
      val t = f(z)
      ((t._1, t._2), (t._3, t._4))
    }

  def divide5[A1, A2, A3, A4, A5, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5])(f: Z => (A1, A2, A3, A4, A5)): F[Z] =
    divide3(tuple2(a1, a2), tuple2(a3, a4), a5){ z =>
      val t = f(z)
      ((t._1, t._2), (t._3, t._4), t._5)
    }

  def divide6[A1, A2, A3, A4, A5, A6, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6])(f: Z => (A1, A2, A3, A4, A5, A6)): F[Z] =
    divide3(tuple2(a1, a2), tuple2(a3, a4), tuple2(a5, a6)){ z =>
      val t = f(z)
      ((t._1, t._2), (t._3, t._4), (t._5, t._6))
    }

  def divide7[A1, A2, A3, A4, A5, A6, A7, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7])(f: Z => (A1, A2, A3, A4, A5, A6, A7)): F[Z] =
    divide3(tuple3(a1, a2, a3), tuple3(a4, a5, a6), a7){ z =>
      val t = f(z)
      ((t._1, t._2, t._3), (t._4, t._5, t._6), t._7)
    }

  def divide8[A1, A2, A3, A4, A5, A6, A7, A8, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8)): F[Z] =
    divide3(tuple3(a1, a2, a3), tuple3(a4, a5, a6), tuple2(a7, a8)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3), (t._4, t._5, t._6), (t._7, t._8))
    }

  def divide9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9)): F[Z] =
    divide3(tuple3(a1, a2, a3), tuple3(a4, a5, a6), tuple3(a7, a8, a9)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3), (t._4, t._5, t._6), (t._7, t._8, t._9))
    }

  def divide10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)): F[Z] =
    divide4(tuple3(a1, a2, a3), tuple3(a4, a5, a6), tuple3(a7, a8, a9), a10){ z =>
      val t = f(z)
      ((t._1, t._2, t._3), (t._4, t._5, t._6), (t._7, t._8, t._9), t._10)
    }

  def divide11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)): F[Z] =
    divide4(tuple3(a1, a2, a3), tuple3(a4, a5, a6), tuple3(a7, a8, a9), tuple2(a10, a11)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3), (t._4, t._5, t._6), (t._7, t._8, t._9), (t._10, t._11))
    }

  def divide12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)): F[Z] =
    divide4(tuple3(a1, a2, a3), tuple3(a4, a5, a6), tuple3(a7, a8, a9), tuple3(a10, a11, a12)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3), (t._4, t._5, t._6), (t._7, t._8, t._9), (t._10, t._11, t._12))
    }

  def divide13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)): F[Z] =
    divide4(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), a13){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), t._13)
    }

  def divide14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)): F[Z] =
    divide4(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple2(a13, a14)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14))
    }

  def divide15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)): F[Z] =
    divide4(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple3(a13, a14, a15)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14, t._15))
    }

  def divide16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)): F[Z] =
    divide4(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple4(a13, a14, a15, a16)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14, t._15, t._16))
    }

  def divide17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)): F[Z] =
    divide5(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple4(a13, a14, a15, a16), a17){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14, t._15, t._16), t._17)
    }

  def divide18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)): F[Z] =
    divide5(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple4(a13, a14, a15, a16), tuple2(a17, a18)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14, t._15, t._16), (t._17, t._18))
    }

  def divide19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)): F[Z] =
    divide5(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple4(a13, a14, a15, a16), tuple3(a17, a18, a19)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14, t._15, t._16), (t._17, t._18, t._19))
    }

  def divide20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)): F[Z] =
    divide5(tuple4(a1, a2, a3, a4), tuple4(a5, a6, a7, a8), tuple4(a9, a10, a11, a12), tuple4(a13, a14, a15, a16), tuple4(a17, a18, a19, a20)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4), (t._5, t._6, t._7, t._8), (t._9, t._10, t._11, t._12), (t._13, t._14, t._15, t._16), (t._17, t._18, t._19, t._20))
    }

  def divide21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20], a21: F[A21])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)): F[Z] =
    divide5(tuple5(a1, a2, a3, a4, a5), tuple5(a6, a7, a8, a9, a10), tuple5(a11, a12, a13, a14, a15), tuple5(a16, a17, a18, a19, a20), a21){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4, t._5), (t._6, t._7, t._8, t._9, t._10), (t._11, t._12, t._13, t._14, t._15), (t._16, t._17, t._18, t._19, t._20), t._21)
    }

  def divide22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20], a21: F[A21], a22: F[A22])(f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)): F[Z] =
    divide5(tuple5(a1, a2, a3, a4, a5), tuple5(a6, a7, a8, a9, a10), tuple5(a11, a12, a13, a14, a15), tuple5(a16, a17, a18, a19, a20), tuple2(a21, a22)){ z =>
      val t = f(z)
      ((t._1, t._2, t._3, t._4, t._5), (t._6, t._7, t._8, t._9, t._10), (t._11, t._12, t._13, t._14, t._15), (t._16, t._17, t._18, t._19, t._20), (t._21, t._22))
    }

  def tuple2[A1, A2](a1: F[A1], a2: F[A2]): F[(A1, A2)] =
    divide2(a1, a2)(identity)

  def tuple3[A1, A2, A3](a1: F[A1], a2: F[A2], a3: F[A3]): F[(A1, A2, A3)] =
    divide3(a1, a2, a3)(identity)

  def tuple4[A1, A2, A3, A4](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[(A1, A2, A3, A4)] =
    divide4(a1, a2, a3, a4)(identity)

  def tuple5[A1, A2, A3, A4, A5](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5]): F[(A1, A2, A3, A4, A5)] =
    divide5(a1, a2, a3, a4, a5)(identity)

  def tuple6[A1, A2, A3, A4, A5, A6](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6]): F[(A1, A2, A3, A4, A5, A6)] =
    divide6(a1, a2, a3, a4, a5, a6)(identity)

  def tuple7[A1, A2, A3, A4, A5, A6, A7](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7]): F[(A1, A2, A3, A4, A5, A6, A7)] =
    divide7(a1, a2, a3, a4, a5, a6, a7)(identity)

  def tuple8[A1, A2, A3, A4, A5, A6, A7, A8](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8]): F[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    divide8(a1, a2, a3, a4, a5, a6, a7, a8)(identity)

  def tuple9[A1, A2, A3, A4, A5, A6, A7, A8, A9](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    divide9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(identity)

  def tuple10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    divide10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(identity)

  def tuple11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    divide11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(identity)

  def tuple12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    divide12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(identity)

  def tuple13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    divide13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(identity)

  def tuple14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    divide14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(identity)

  def tuple15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    divide15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(identity)

  def tuple16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    divide16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(identity)

  def tuple17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    divide17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(identity)

  def tuple18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    divide18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(identity)

  def tuple19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    divide19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(identity)

  def tuple20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    divide20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(identity)

  def tuple21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20], a21: F[A21]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    divide21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(identity)

  def tuple22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20], a21: F[A21], a22: F[A22]): F[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] =
    divide22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)(identity)

  def deriving2[A1, A2, Z](f: Z => (A1, A2))(implicit a1: F[A1], a2: F[A2]): F[Z] =
    divide2(a1, a2)(f)

  def deriving3[A1, A2, A3, Z](f: Z => (A1, A2, A3))(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] =
    divide3(a1, a2, a3)(f)

  def deriving4[A1, A2, A3, A4, Z](f: Z => (A1, A2, A3, A4))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] =
    divide4(a1, a2, a3, a4)(f)

  def deriving5[A1, A2, A3, A4, A5, Z](f: Z => (A1, A2, A3, A4, A5))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5]): F[Z] =
    divide5(a1, a2, a3, a4, a5)(f)

  def deriving6[A1, A2, A3, A4, A5, A6, Z](f: Z => (A1, A2, A3, A4, A5, A6))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6]): F[Z] =
    divide6(a1, a2, a3, a4, a5, a6)(f)

  def deriving7[A1, A2, A3, A4, A5, A6, A7, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7]): F[Z] =
    divide7(a1, a2, a3, a4, a5, a6, a7)(f)

  def deriving8[A1, A2, A3, A4, A5, A6, A7, A8, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8]): F[Z] =
    divide8(a1, a2, a3, a4, a5, a6, a7, a8)(f)

  def deriving9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9]): F[Z] =
    divide9(a1, a2, a3, a4, a5, a6, a7, a8, a9)(f)

  def deriving10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10]): F[Z] =
    divide10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(f)

  def deriving11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11]): F[Z] =
    divide11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(f)

  def deriving12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12]): F[Z] =
    divide12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(f)

  def deriving13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13]): F[Z] =
    divide13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(f)

  def deriving14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14]): F[Z] =
    divide14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(f)

  def deriving15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15]): F[Z] =
    divide15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(f)

  def deriving16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16]): F[Z] =
    divide16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(f)

  def deriving17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17]): F[Z] =
    divide17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(f)

  def deriving18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18]): F[Z] =
    divide18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(f)

  def deriving19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19]): F[Z] =
    divide19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(f)

  def deriving20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20]): F[Z] =
    divide20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(f)

  def deriving21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20], a21: F[A21]): F[Z] =
    divide21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(f)

  def deriving22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](f: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22))(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4], a5: F[A5], a6: F[A6], a7: F[A7], a8: F[A8], a9: F[A9], a10: F[A10], a11: F[A11], a12: F[A12], a13: F[A13], a14: F[A14], a15: F[A15], a16: F[A16], a17: F[A17], a18: F[A18], a19: F[A19], a20: F[A20], a21: F[A21], a22: F[A22]): F[Z] =
    divide22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)(f)


  trait DivideLaw extends ContravariantLaw {
    protected[this] def delta[A]: A => (A, A) = a => (a, a)
    def composition[A](a1: F[A], a2: F[A], a3: F[A])(implicit E: Equal[F[A]]): Boolean = {
      val x = divide(divide(a1, a2)(delta[A]), a3)(delta[A])
      val y = divide(a1, divide(a2, a3)(delta[A]))(delta[A])
      E.equal(x, y)
    }
  }

  def divideLaw = new DivideLaw {}

  ////
  val divideSyntax: scalaz.syntax.DivideSyntax[F] =
    new scalaz.syntax.DivideSyntax[F] { def F = Divide.this }
}

object Divide {
  @inline def apply[F[_]](implicit F: Divide[F]): Divide[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Divide[G]): Divide[F] =
    new IsomorphismDivide[F, G] {
      override def G: Divide[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}
