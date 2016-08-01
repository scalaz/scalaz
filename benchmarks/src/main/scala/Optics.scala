package scalaz
package benchmarks


object Optics extends LensModel {
  val init = oi(42)

  object Scalaz {
    import data._
    import data.Lens._

    def _lens = slens[O0, O1](_.o)(o => p => o.copy(o = p))

    val _01 = slens[O0, O1](_.o)(o => p => o.copy(o = p))
    val _12 = slens[O1, O2](_.o)(o => p => o.copy(o = p))
    val _23 = slens[O2, O3](_.o)(o => p => o.copy(o = p))
    val _34 = slens[O3, O4](_.o)(o => p => o.copy(o = p))
    val _45 = slens[O4, O5](_.o)(o => p => o.copy(o = p))
    val _56 = slens[O5, O6](_.o)(o => p => o.copy(o = p))
    val _67 = slens[O6, O7](_.o)(o => p => o.copy(o = p))
    val _78 = slens[O7, O8](_.o)(o => p => o.copy(o = p))
    val _89 = slens[O8, O9](_.o)(o => p => o.copy(o = p))
    val _i  = slens[O9, Int](_.i)(o => i => o.copy(i = i))

    val _baseline = slens[O0, Int](_.o.o.o.o.o.o.o.o.o.i)(o => i =>
        o.copy(o =
          o.o.copy(o =
            o.o.o.copy(o =
              o.o.o.o.copy(o =
                o.o.o.o.o.copy(o =
                  o.o.o.o.o.o.copy(o =
                    o.o.o.o.o.o.o.copy(o =
                      o.o.o.o.o.o.o.o.copy(o =
                        o.o.o.o.o.o.o.o.o.copy(o =
                          o.o.o.o.o.o.o.o.o.o.copy(i = i)))))))))))

    val _telephoto = Optic.telephotoA(_01, _12, _23, _34, _45, _56, _67, _78, _89, _i)

    val _telephotoHList = {
      import data.HList._

      Telephoto(
        HCons(_i, HCons(_89, HCons(_78, HCons(_67, HCons(_56,
          HCons(_45, HCons(_34, HCons(_23, HCons(_12, HCons(_01, HNil))))))))))
      ).optic
    }
  }

  object Monocle {
    import monocle._

    def _lens = Lens[O0, O1](_.o)(p => o => o.copy(o = p))

    val _01 = Lens[O0, O1](_.o)(p => o => o.copy(o = p))
    val _12 = Lens[O1, O2](_.o)(p => o => o.copy(o = p))
    val _23 = Lens[O2, O3](_.o)(p => o => o.copy(o = p))
    val _34 = Lens[O3, O4](_.o)(p => o => o.copy(o = p))
    val _45 = Lens[O4, O5](_.o)(p => o => o.copy(o = p))
    val _56 = Lens[O5, O6](_.o)(p => o => o.copy(o = p))
    val _67 = Lens[O6, O7](_.o)(p => o => o.copy(o = p))
    val _78 = Lens[O7, O8](_.o)(p => o => o.copy(o = p))
    val _89 = Lens[O8, O9](_.o)(p => o => o.copy(o = p))
    val _i  = Lens[O9, Int](_.i)(i => o => o.copy(i = i))

    val _compose = _01 composeLens _12 composeLens _23 composeLens _34 composeLens _45 composeLens _56 composeLens _67 composeLens _78 composeLens _89 composeLens _i
  }
}
