package scalaz
package newtypes

sealed trait ZipStream[A] {
  val value: Stream[A]
}

object ZipStream extends ZipStreams

trait ZipStreams {
  import wrap.StreamW._

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

  implicit val ZipStreamFunctor: Functor[ZipStream] =
    implicitly[Functor[Stream]].deriving[ZipStream]

  implicit val ZipStreamPointed: Pointed[ZipStream] =
    implicitly[Pointed[Stream]].deriving[ZipStream]

  implicit def ZipStreamPointedFunctor: PointedFunctor[ZipStream] =
    implicitly[PointedFunctor[Stream]].deriving[ZipStream]

  implicit val ZipStreamApplic: Applic[ZipStream] = new Applic[ZipStream] {
    def applic[A, B](f: ZipStream[A => B]) =
      a => {
        val ff = f.value
        val aa = a.value
        (if (ff.isEmpty || aa.isEmpty) Stream.empty
        else Stream.cons((ff.head)(aa.head), applic(ff.tail ʐ)(aa.tail ʐ).value)) ʐ
      }
  }

  implicit def ZipStreamApplicative: Applicative[ZipStream] =
    Applicative.applicative

  implicit def ZipStreamApplicFunctor: ApplicFunctor[ZipStream] =
    ApplicFunctor.applicFunctor
    
  implicit def ZipStreamEach: Each[ZipStream] =
    implicitly[Each[Stream]].deriving[ZipStream]

  implicit def ZipStreamFoldr: Foldr[ZipStream] =
    implicitly[Foldr[Stream]].deriving[ZipStream]

  implicit def ZipStreamFoldl: Foldl[ZipStream] =
    implicitly[Foldl[Stream]].deriving[ZipStream]

  implicit def ZipStreamFoldable: Foldable[ZipStream] =
    implicitly[Foldable[Stream]].deriving[ZipStream]

  implicit def ZipStreamIndex: Index[ZipStream] =
    implicitly[Index[Stream]].deriving[ZipStream]

  implicit def ZipStreamLength: Length[ZipStream] =
    implicitly[Length[Stream]].deriving[ZipStream]
  


}
