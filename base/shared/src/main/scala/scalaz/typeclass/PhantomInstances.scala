package scalaz
package typeclass

import scalaz.data.Const

trait PhantomInstances { instances =>
  implicit def const[R]: Phantom[Const[R, ?]] = new PhantomClass.MapContramap[Const[R, ?]] {

    def pmap[A, B](ma: Const[R, A]): Const[R, B] = ma.retag[B]

  }
}
