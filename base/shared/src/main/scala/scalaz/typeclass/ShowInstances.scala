package scalaz
package typeclass

trait ShowInstances {

  implicit final def string: Show[String] = s => s


  implicit final def contravariant: Contravariant[Show] =
    new Contravariant[Show] {
      def contramap[A, B](r: Show[A])(f: B => A): Show[B] =
        b => r.show(f(b))
    }
}