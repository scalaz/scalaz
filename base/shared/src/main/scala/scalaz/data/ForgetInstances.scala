package scalaz
package data

import typeclass.StrongClass

trait ForgetInstances { self =>
  implicit def strong[A]: Strong[Forget[A, ?, ?]] =
    instanceOf(new StrongClass[Forget[A, ?, ?]] {
      override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] =
        Forget[A, D, E](fdb andThen fbc.forget)

      override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
        Forget[A, D, C](fdb andThen fbc.forget)

      override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
        fbc.retag[D]

      override def first[B, C, D](pab: Forget[A, B, C]): Forget[A, (B, D), (C, D)] =
        Forget[A, (B, D), (C, D)](bd => pab.forget(bd._1))

      override def second[B, C, D](pab: Forget[A, B, C]): Forget[A, (D, B), (D, C)] =
        Forget[A, (D, B), (D, C)](bd => pab.forget(bd._2))
    })
}
