package scalaz
package newtypes

sealed trait ZipStream[A] extends Pimp[Stream[A]]

object ZipStream extends ZipStreams

trait ZipStreams {
  implicit def ZipStreamUnpack[A]: Unpack[ZipStream[A], Stream[A]] = new Unpack[ZipStream[A], Stream[A]] {
    val unpack = (_: ZipStream[A]).value
  }

  implicit def ZipStreamPack[A]: Pack[ZipStream[A], Stream[A]] = new Pack[ZipStream[A], Stream[A]] {
    val pack = (b: Stream[A]) => new ZipStream[A] {
      val value = b
    }
  }

  implicit def ZipStreamNewtype[A]: Newtype[ZipStream[A], Stream[A]] =
    Newtype.newtype

  implicit def ZipStreamShow[A: Show]: Show[ZipStream[A]] =
    implicitly[Show[Stream[A]]] contramap ((_: ZipStream[A]).value)

  implicit def ZipStreamEqual[A: Equal]: Equal[ZipStream[A]] =
    implicitly[Equal[Stream[A]]] contramap ((_: ZipStream[A]).value)

  implicit def ZipStreamOrder[A: Order]: Order[ZipStream[A]] =
    implicitly[Order[Stream[A]]] contramap ((_: ZipStream[A]).value)

}
