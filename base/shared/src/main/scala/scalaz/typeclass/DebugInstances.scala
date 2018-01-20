package scalaz
package typeclass

trait DebugInstances {
  implicit final def stringDebug: Debug[String] = s => s

  implicit final def contravariantDebug: Contravariant[Debug] =
    new Contravariant[Debug] {
      def contramap[A, B](r: Debug[A])(f: B => A): Debug[B] =
        b => r.debug(f(b))
    }
}