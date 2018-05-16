package scalaz
package ct

import scalaz.data.Const

trait PhantomInstances {

  implicit def const[R]: Phantom[Const[R, ?]] =
    instanceOf(new PhantomClass.DeriveMapContramap[Const[R, ?]] {
      def pmap[A, B](ma: Const[R, A]): Const[R, B] = ma.retag[B]
    })
}
