package scalaz
package debug

import scalaz.ct.ContravariantClass

trait DebugInstances {
  implicit final def contravariantDebug: Contravariant[DebugClass] =
    instanceOf(new ContravariantClass[DebugClass] {
      def contramap[A, B](r: DebugClass[A])(f: B => A): DebugClass[B] =
        b => r.debug(f(b))
    })
}
