package scalaz
package typeclass

trait DebugInstances {
  implicit final def stringDebug: Debug[String] = Debug.fromDebugs(s => s)

  implicit final def contravariantDebug: Contravariant[Debug] =
    new Contravariant[Debug] {
      def contramap[A, B](r: Debug[A])(f: B => A): Debug[B] =
        Debug.fromDebugs(b => r.debugs(f(b)))
    }
}