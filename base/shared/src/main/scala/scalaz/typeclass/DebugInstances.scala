package scalaz
package typeclass

import scalaz.ct.ContravariantClass

trait DebugInstances {
  implicit final def stringDebug: Debug[String] = instanceOf[DebugClass[String]](s => s)

  implicit final def contravariantDebug: Contravariant[DebugClass] =
    instanceOf(new ContravariantClass[DebugClass] {
      def contramap[A, B](r: DebugClass[A])(f: B => A): DebugClass[B] =
        b => r.debug(f(b))
    })
}
