package scalaz
package newtypes

sealed trait ZipStream[A] {
  val value: Stream[A]
}

object ZipStream extends ZipStreams

trait ZipStreams {
  implicit def ZipStream_^*^[A]: ^*^[ZipStream[A], Stream[A]] =
    ^*^.^*^(_.value, b => new ZipStream[A] {
      val value = b
    })

  implicit def ZipStream_^**^ : ^**^[ZipStream, Stream] =
    new ^**^[ZipStream, Stream] {
      def unpack[A] = _.value
      def pack[A] = b => new ZipStream[A] {
      val value = b
    }
  }

  implicit def ZipStreamShow[A: Show]: Show[ZipStream[A]] =
    implicitly[Show[Stream[A]]] contramap ((_: ZipStream[A]).value)

  implicit def ZipStreamEqual[A: Equal]: Equal[ZipStream[A]] =
    implicitly[Equal[Stream[A]]] contramap ((_: ZipStream[A]).value)

  implicit def ZipStreamOrder[A: Order]: Order[ZipStream[A]] =
    implicitly[Order[Stream[A]]] contramap ((_: ZipStream[A]).value)

}
