package scalaz
package typeclass

trait ShowInstances {
  implicit final def stringShow: Show[String] = Show.fromShows(s => s)

  implicit final def contravariantShow: Contravariant[Show] =
    new Contravariant[Show] {
      def contramap[A, B](r: Show[A])(f: B => A): Show[B] =
        Show.fromShow(b => r.show(f(b)))
    }
}