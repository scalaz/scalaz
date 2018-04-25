package scalaz

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.reflect.ClassTag

abstract class ImmutableArrayInstances1 {

  implicit def canBuildFrom[T](implicit m: ClassTag[T]): CanBuildFrom[ImmutableArray[_], T, ImmutableArray[T]] =
    new CanBuildFrom[ImmutableArray[_], T, ImmutableArray[T]] {
      def apply(from: ImmutableArray[_]): Builder[T, ImmutableArray[T]] =
        ImmutableArray.newBuilder(m)

      def apply: Builder[T, ImmutableArray[T]] =
        ImmutableArray.newBuilder(m)
    }

  implicit def canBuildFromChar(implicit m: ClassTag[Char]): CanBuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] =
    new CanBuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] {
      def apply(from: ImmutableArray[_]): Builder[Char, ImmutableArray[Char]] =
        ImmutableArray.newStringArrayBuilder

      def apply: Builder[Char, ImmutableArray[Char]] =
        ImmutableArray.newStringArrayBuilder
    }

}
