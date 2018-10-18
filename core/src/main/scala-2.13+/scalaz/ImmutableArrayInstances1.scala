package scalaz

import scala.collection.BuildFrom
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

abstract class ImmutableArrayInstances1 {

  implicit def buildFrom[T](implicit m: ClassTag[T]): BuildFrom[ImmutableArray[_], T, ImmutableArray[T]] =
    new BuildFrom[ImmutableArray[_], T, ImmutableArray[T]] {
      override def newBuilder(from: ImmutableArray[_]): Builder[T, ImmutableArray[T]] =
        ImmutableArray.newBuilder(m)

      def fromSpecific(from: ImmutableArray[_])(it: IterableOnce[T]): ImmutableArray[T] =
        ImmutableArray.fromArray(it.toArray)
    }

  implicit val buildFromChar: BuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] =
    new BuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] {
      override def newBuilder(from: ImmutableArray[_]): Builder[Char, ImmutableArray[Char]] =
        ImmutableArray.newStringArrayBuilder

      def fromSpecific(from: ImmutableArray[_])(it: IterableOnce[Char]): ImmutableArray[Char] =
        ImmutableArray.fromArray(it.toArray)
    }

}
