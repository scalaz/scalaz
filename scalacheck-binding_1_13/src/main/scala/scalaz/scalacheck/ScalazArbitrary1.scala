package scalaz
package scalacheck

import org.scalacheck.Cogen
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.AllInstances._
import scalaz.syntax.foldable._

abstract class ScalazArbitrary1 {

  implicit def cogenIList[A: Cogen]: Cogen[IList[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenMaybe[A: Cogen]: Cogen[Maybe[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  implicit def cogenLazyOption[A: Cogen]: Cogen[LazyOption[A]] =
    Cogen[Option[A]].contramap(_.toOption)

  implicit def cogenThese[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \&/ B] =
    Cogen[(A \/ B) \/ (A, B)].contramap{
      case x @ \&/.Both(a, b) =>
        \/-((a, b))
      case \&/.This(a) =>
        -\/(-\/(a))
      case \&/.That(b) =>
        -\/(\/-(b))
    }

  implicit def cogenLazyEither[A: Cogen, B: Cogen]: Cogen[LazyEither[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit def cogenDisjunction[A, B](implicit A: Cogen[A], B: Cogen[B]): Cogen[A \/ B] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit def cogenValidation[A: Cogen, B: Cogen]: Cogen[Validation[A, B]] =
    Cogen[Either[A, B]].contramap(_.toEither)

  implicit val cogenOrdering: Cogen[Ordering] =
    Cogen[Int].contramap {
      case Ordering.GT => 0
      case Ordering.EQ => 1
      case Ordering.LT => 2
    }

  implicit def cogenOneOr[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneOr[F, A]] =
    Cogen[F[A] \/ A].contramap(_.run)

  implicit def cogenOneAnd[F[_], A: Cogen](implicit F: Cogen[F[A]]): Cogen[OneAnd[F, A]] =
    Cogen[(A, F[A])].contramap(a => (a.head, a.tail))

  implicit def cogenISet[A: Cogen]: Cogen[ISet[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenIMap[A: Cogen, B: Cogen]: Cogen[A ==>> B] =
    Cogen[List[(A, B)]].contramap(_.toList)

  implicit def cogenDList[A: Cogen]: Cogen[DList[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenEphemeralStream[A: Cogen]: Cogen[EphemeralStream[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenHeap[A: Cogen]: Cogen[Heap[A]] =
    Cogen[Stream[A]].contramap(_.toUnsortedStream)

  implicit def cogenDequeue[A: Cogen]: Cogen[Dequeue[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenNonEmptyList[A: Cogen]: Cogen[NonEmptyList[A]] =
    Cogen[(A, IList[A])].contramap(nel => (nel.head, nel.tail))

  implicit def cogenIndSeq[A: Cogen]: Cogen[IndSeq[A]] =
    Cogen[List[A]].contramap(Foldable[IndSeq].toList)

  implicit def cogenDiev[A: Cogen]: Cogen[Diev[A]] =
    Cogen[Vector[(A, A)]].contramap(_.intervals)

  implicit def cogenCoproduct[F[_], G[_], A](implicit A: Cogen[F[A] \/ G[A]]): Cogen[Coproduct[F, G, A]] =
    Cogen[F[A] \/ G[A]].contramap(_.run)

  implicit def cogenConst[A, B](implicit A: Cogen[A]): Cogen[Const[A, B]] =
    A.contramap(_.getConst)

  implicit def cogenZipper[A](implicit A: Cogen[A]): Cogen[Zipper[A]] =
    Cogen[(Stream[A], A, Stream[A])].contramap(z => (z.lefts, z.focus, z.rights))

  implicit def cogenTracedT[W[_], A, B](implicit W: Cogen[W[A => B]]): Cogen[TracedT[W, A, B]] =
    W.contramap(_.run)

  implicit def cogenIndexedStoreT[F[_], I: Cogen, A, B](implicit F: Cogen[F[A => B]]): Cogen[IndexedStoreT[F, I, A, B]] =
    Cogen[(F[A => B], I)].contramap(_.run)

  implicit def cogenIndexedContsT[W[_], M[_], R, O, A](implicit F: Cogen[W[A => M[O]] => M[R]]): Cogen[IndexedContsT[W, M, R, O, A]] =
    F.contramap(_.run)

  implicit def cogenEndo[A](implicit A: Cogen[A => A]): Cogen[Endo[A]] =
    A.contramap(_.run)

  implicit def cogenEndomorphic[F[_, _], A](implicit F: Cogen[F[A, A]]): Cogen[Endomorphic[F, A]] =
    F.contramap(_.run)

  implicit def cogenKleisli[F[_], A, B](implicit F: Cogen[A => F[B]]): Cogen[Kleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenCokleisli[F[_], A, B](implicit F: Cogen[F[A] => B]): Cogen[Cokleisli[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenNullResult[A, B](implicit F: Cogen[A => Option[B]]): Cogen[NullResult[A, B]] =
    F.contramap(_.apply)

  implicit def cogenNullArgument[A, B](implicit F: Cogen[Option[A] => B]): Cogen[NullArgument[A, B]] =
    F.contramap(_.apply)

  implicit def cogenContravariantCoyoneda[F[_]: Contravariant, A](implicit F: Cogen[F[A]]): Cogen[ContravariantCoyoneda[F, A]] =
    Cogen[F[A]].contramap(_.run)

  implicit def cogenEitherT[F[_], A, B](implicit F: Cogen[F[A \/ B]]): Cogen[EitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenLazyEitherT[F[_], A, B](implicit F: Cogen[F[LazyEither[A, B]]]): Cogen[LazyEitherT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTheseT[F[_], A, B](implicit F: Cogen[F[A \&/ B]]): Cogen[TheseT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenMaybeT[F[_], A](implicit F: Cogen[F[Maybe[A]]]): Cogen[MaybeT[F, A]] =
    F.contramap(_.run)

  implicit def cogenStreamT[F[_]: Monad, A](implicit F: Cogen[F[Stream[A]]]): Cogen[StreamT[F, A]] =
    F.contramap(_.toStream)

  implicit def cogenOptionT[F[_], A](implicit F: Cogen[F[Option[A]]]): Cogen[OptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenLazyOptionT[F[_], A](implicit F: Cogen[F[LazyOption[A]]]): Cogen[LazyOptionT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIdT[F[_], A](implicit F: Cogen[F[A]]): Cogen[IdT[F, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedReaderWriterStateT[F[_]: Monad, R, W, S1, S2, A](implicit F: Cogen[(R, S1) => F[(W, A, S2)]]): Cogen[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedStateT[F[_]: Monad, S1, S2, A](implicit F: Cogen[S1 => F[(S2, A)]]): Cogen[IndexedStateT[F, S1, S2, A]] =
    F.contramap(s => s.apply(_))

  implicit def cogenWriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[WriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenUnwriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[UnwriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTree[A: Cogen]: Cogen[Tree[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenTreeLoc[A: Cogen]: Cogen[TreeLoc[A]] =
    Divide[Cogen].deriving4(Function.unlift(TreeLoc.unapply))

  implicit def cogenStrictTree[A: Cogen]: Cogen[StrictTree[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenCoyoneda[F[_]: Functor, A](implicit F: Cogen[F[A]]): Cogen[Coyoneda[F, A]] =
    F.contramap(_.run)

  private[this] def nameToValue[A]: Name[A] => A = _.value

  implicit def cogenNeed[A](implicit A: Cogen[A]): Cogen[Need[A]] =
    A.contramap(nameToValue)

  implicit def cogenValue[A](implicit A: Cogen[A]): Cogen[Value[A]] =
    A.contramap(nameToValue)

  implicit def cogenName[A](implicit A: Cogen[A]): Cogen[Name[A]] =
    A.contramap(nameToValue)

  implicit def cogenImmutableArray[A: Cogen]: Cogen[ImmutableArray[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenEither3[A1: Cogen, A2: Cogen, A3: Cogen]: Cogen[Either3[A1, A2, A3]] =
    Cogen[A1 \/ A2 \/ A3].contramap{
      case Left3(a) => -\/(-\/(a))
      case Middle3(a) => -\/(\/-(a))
      case Right3(a) => \/-(a)
    }

  implicit def cogenLazyTuple2[A1: Cogen, A2: Cogen]: Cogen[LazyTuple2[A1, A2]] =
    Cogen[(A1, A2)].contramap(t => (t._1, t._2))

  implicit def cogenLazyTuple3[A1: Cogen, A2: Cogen, A3: Cogen]: Cogen[LazyTuple3[A1, A2, A3]] =
    Cogen[(A1, A2, A3)].contramap(t => (t._1, t._2, t._3))

  implicit def cogenLazyTuple4[A1: Cogen, A2: Cogen, A3: Cogen, A4: Cogen]: Cogen[LazyTuple4[A1, A2, A3, A4]] =
    Cogen[(A1, A2, A3, A4)].contramap(t => (t._1, t._2, t._3, t._4))
}
