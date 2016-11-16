package scalaz
package scalacheck

import java.math.BigInteger
import org.scalacheck.{Gen, Arbitrary}
import collection.mutable.ArraySeq
import reflect.ClassTag

/**
 * Instances of {@link scalacheck.Arbitrary} for many types in Scalaz.
 */
object ScalazArbitrary extends ScalazArbitraryPlatform {
  import Scalaz._
  import Tags._
  import Arbitrary._
  import Gen._
  import ScalaCheckBinding._

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def monoidCoproductArbitrary[M: Arbitrary, N: Arbitrary]: Arbitrary[M :+: N] =
    Functor[Arbitrary].map(arb[Vector[M \/ N]])(new :+:(_))

  /** @since 7.0.3 */
  implicit def theseArb[A: Arbitrary, B: Arbitrary]: Arbitrary[A \&/ B] =
    Arbitrary(Gen.oneOf(
      ^(arbitrary[A], arbitrary[B])(\&/.Both(_, _)),
      arbitrary[A].map(\&/.This(_)),
      arbitrary[B].map(\&/.That(_))
    ))

  implicit def EphemeralStreamArbitrary[A : Arbitrary] =
    Functor[Arbitrary].map(arb[Stream[A]])(EphemeralStream.fromStream[A](_))

  implicit def ImmutableArrayArbitrary[A : Arbitrary : ClassTag] =
    Functor[Arbitrary].map(arb[Array[A]])(ImmutableArray.fromArray[A](_))

  implicit def ValueArbitrary[A: Arbitrary]: Arbitrary[Value[A]] = Functor[Arbitrary].map(arb[A])(a => Value(a))
  implicit def NameArbitrary[A: Arbitrary]: Arbitrary[Name[A]] = Functor[Arbitrary].map(arb[A])(a => Name(a))
  implicit def NeedArbitrary[A: Arbitrary]: Arbitrary[Need[A]] = Functor[Arbitrary].map(arb[A])(a => Need(a))

  implicit val UnitArbitrary: Arbitrary[Unit] = Arbitrary(const(()))

  implicit val AlphaArbitrary: Arbitrary[Alpha] = Arbitrary(oneOf(Alpha.alphas))

  implicit val BooleanConjunctionArbitrary: Arbitrary[Boolean @@ Conjunction] = Functor[Arbitrary].map(arb[Boolean])(_.conjunction)

  implicit val arbBigInt: Arbitrary[BigInt] = Apply[Arbitrary].apply2[Int, Int, BigInt](arb[Int], arb[Int])(_ + _)

  implicit val arbBigInteger: Arbitrary[BigInteger] = Functor[Arbitrary].map(arb[BigInt])(_.bigInteger)

  implicit val BigIntegerMultiplicationArbitrary: Arbitrary[BigInteger @@ Multiplication] =
    Tag.subst[BigInteger, Arbitrary, Multiplication](arb[BigInteger])

  implicit val BigIntMultiplicationArbitrary: Arbitrary[BigInt @@ Multiplication] = Tag.subst(arb[BigInt])

  implicit val ByteMultiplicationArbitrary: Arbitrary[Byte @@ Multiplication] = Tag.subst(arb[Byte])

  implicit val CharMultiplicationArbitrary: Arbitrary[Char @@ Multiplication] = Tag.subst(arb[Char])

  implicit val ShortMultiplicationArbitrary: Arbitrary[Short @@ Multiplication] = Tag.subst(arb[Short])

  implicit val IntMultiplicationArbitrary: Arbitrary[Int @@ Multiplication] = Tag.subst(arb[Int])

  implicit val LongMultiplicationArbitrary: Arbitrary[Long @@ Multiplication] = Tag.subst(arb[Long])

  implicit val FloatMultiplicationArbitrary: Arbitrary[Float @@ Multiplication] = Tag.subst(arb[Float])

  implicit val DoubleMultiplicationArbitrary: Arbitrary[Double @@ Multiplication] = Tag.subst(arb[Double])

  implicit val DigitArbitrary: Arbitrary[Digit] = Arbitrary(oneOf(Digit.digits))

  import NonEmptyList._
  implicit def NonEmptyListArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Apply[Arbitrary].apply2[A, IList[A], NonEmptyList[A]](arb[A], ilistArbitrary)(nel(_, _))


  /** @since 7.0.3 */
  implicit def OneAndArbitrary[F[_], A](implicit A: Arbitrary[A], FA: Arbitrary[F[A]]
                                        ): Arbitrary[OneAnd[F, A]] =
    Apply[Arbitrary].apply2(arb[A], arb[F[A]])(OneAnd.apply)

  /** @since 7.0.3 */
  implicit def OneOrArbitrary[F[_], A](implicit A: Arbitrary[A], FA: Arbitrary[F[A]]): Arbitrary[OneOr[F, A]] =
    Functor[Arbitrary].map(arb[F[A] \/ A])(OneOr(_))

  /** @since 7.1.0 */
  implicit def Arbitrary_==>>[A, B](implicit o: Order[A], A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[A ==>> B] =
    Functor[Arbitrary].map(arb[List[(A, B)]])(as => ==>>.fromList(as))

  implicit def Arbitrary_ISet[A](implicit o: Order[A], A: Arbitrary[A]): Arbitrary[ISet[A]] =
    Functor[Arbitrary].map(arb[List[A]])(as => ISet.fromList(as))

  implicit def Arbitrary_Maybe[A: Arbitrary]: Arbitrary[Maybe[A]] =
    Functor[Arbitrary].map(arb[Option[A]])(Maybe.fromOption)

  import scalaz.Ordering._
  implicit val OrderingArbitrary: Arbitrary[Ordering] = Arbitrary(oneOf(LT, EQ, GT))

  private[this] def withSize[A](size: Int)(f: Int => Gen[A]): Gen[Stream[A]] = {
    Applicative[Gen].sequence(
      Stream.fill(size)(Gen.choose(1, size))
    ).flatMap { s =>
      val ns = Traverse[Stream].traverseS(s) { n =>
        for {
          sum <- State.get[Int]
          r <- if (sum >= size) {
            State.state[Int, Option[Int]](None)
          } else if ((sum + n) > size) {
            State((s: Int) => (s + n) -> Option(size - sum))
          } else {
            State((s: Int) => (s + n) -> Option(n))
          }
        } yield r
      }.eval(0).flatten

      Applicative[Gen].sequence(ns.map(f))
    }
  }

  private[scalaz] def treeGenSized[A: NotNothing](size: Int)(implicit A: Arbitrary[A]): Gen[Tree[A]] =
    size match {
      case n if n <= 1 =>
        A.arbitrary.map(a => Tree.Leaf(a))
      case 2 =>
        arb[(A, A)].arbitrary.map{ case (a1, a2) =>
          Tree.Node(a1, Stream(Tree.Leaf(a2)))
        }
      case 3 =>
        arb[(A, A, A)].arbitrary.flatMap{ case (a1, a2, a3) =>
          Gen.oneOf(
            Tree.Node(a1, Stream(Tree.Leaf(a2), Tree.Leaf(a3))),
            Tree.Node(a1, Stream(Tree.Node(a2, Stream(Tree.Leaf(a3)))))
          )
        }
      case _ =>
        withSize(size - 1)(treeGenSized[A]).flatMap{ as =>
          A.arbitrary.map(a => Tree.Node(a, as))
        }
    }

  implicit def TreeArbitrary[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(Gen.sized(n =>
      if (n < 1) treeGenSized[A](0)
      else Gen.choose(1, n).flatMap(treeGenSized[A])
    ))

  private[scalaz] def strictTreeGenSized[A: NotNothing](size: Int)(implicit A: Arbitrary[A]): Gen[StrictTree[A]] =
    size match {
      case n if n <= 1 =>
        A.arbitrary.map(a => StrictTree.Leaf(a))
      case 2 =>
        arb[(A, A)].arbitrary.map{ case (a1, a2) =>
          StrictTree(a1, Vector(StrictTree.Leaf(a2)))
        }
      case 3 =>
        arb[(A, A, A)].arbitrary.flatMap{ case (a1, a2, a3) =>
          Gen.oneOf(
            StrictTree(a1, Vector(StrictTree.Leaf(a2), StrictTree.Leaf(a3))),
            StrictTree(a1, Vector(StrictTree(a2, Vector(StrictTree.Leaf(a3)))))
          )
        }
      case _ =>
        withSize(size - 1)(strictTreeGenSized[A]).flatMap{ as =>
          A.arbitrary.map(a => StrictTree(a, as.toVector))
        }
    }

  implicit def StrictTreeArbitrary[A: Arbitrary]: Arbitrary[StrictTree[A]] =
    Arbitrary(Gen.sized(n =>
      if (n < 1) strictTreeGenSized[A](0)
      else Gen.choose(1, n).flatMap(strictTreeGenSized[A])
    ))

  private[scalaz] def treeLocGenSized[A: NotNothing](size: Int)(implicit A: Arbitrary[A]): Gen[TreeLoc[A]] = {
    def forest(n: Int): Gen[TreeLoc.TreeForest[A]] =
      withSize(n)(treeGenSized[A])

    val parent: Int => Gen[TreeLoc.Parent[A]] = { n =>
      Gen.choose(0, n - 1).flatMap { x1 =>
        Apply[Gen].tuple3(
          forest(x1), A.arbitrary, forest(n - x1 - 1)
        )
      }
    }

    for{
      a <- Gen.choose(1, size max 1)
      b = size - a
      aa <- Gen.choose(1, a)
      ba <- Gen.choose(0, b max 0)
      t <- Apply[Gen].apply4(
        treeGenSized[A](aa),
        forest(a - aa),
        forest(ba),
        withSize(b - ba)(parent)
      )(TreeLoc.apply[A])
    } yield t
  }

  implicit def IterableArbitrary[A: Arbitrary]: Arbitrary[Iterable[A]] =
      Apply[Arbitrary].apply2[A, List[A], Iterable[A]](arb[A], arb[List[A]])((a, list) => a :: list)

  implicit def TreeLocArbitrary[A: Arbitrary]: Arbitrary[TreeLoc[A]] =
    Arbitrary(Gen.sized(n =>
      Gen.choose(0, n).flatMap(treeLocGenSized[A])
    ))

  implicit def DisjunctionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A \/ B] =
    Functor[Arbitrary].map(arb[Either[A, B]]) {
      case Left(a) => -\/(a)
      case Right(b) => \/-(b)
    }

  implicit def ValidationArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Validation[A, B]] =
    Functor[Arbitrary].map(arb[A \/ B])(_.validation)

//  implicit def ZipStreamArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ZipStream[A]] = arb[Stream[A]] ∘ ((s: Stream[A]) => s.ʐ)

  implicit def Tuple1Arbitrary[A: Arbitrary]: Arbitrary[Tuple1[A]] = Functor[Arbitrary].map(arb[A])(Tuple1(_))

  implicit def Function0Arbitrary[A: Arbitrary]: Arbitrary[() => A] = Functor[Arbitrary].map(arb[A])(() => _)

  implicit def FirstOptionArbitrary[A: Arbitrary]: Arbitrary[Option[A] @@ First] = Functor[Arbitrary].map(arb[Option[A]])(_.first)

  implicit def LastOptionArbitrary[A: Arbitrary]: Arbitrary[Option[A] @@ Last] = Functor[Arbitrary].map(arb[Option[A]])(_.last)

  implicit def MinOptionArbitrary[A: Arbitrary]: Arbitrary[MinOption[A]] = Tag.subst(arb[Option[A]])

  implicit def MaxOptionArbitrary[A: Arbitrary]: Arbitrary[MaxOption[A]] = Tag.subst(arb[Option[A]])

  implicit def FirstMaybeArbitrary[A: Arbitrary]: Arbitrary[Maybe[A] @@ First] = Functor[Arbitrary].map(arb[Maybe[A]])(_.first)

  implicit def LastMaybeArbitrary[A: Arbitrary]: Arbitrary[Maybe[A] @@ Last] = Functor[Arbitrary].map(arb[Maybe[A]])(_.last)

  implicit def MinMaybeArbitrary[A: Arbitrary]: Arbitrary[MinMaybe[A]] = Tag.subst(arb[Maybe[A]])

  implicit def MaxMaybeArbitrary[A: Arbitrary]: Arbitrary[MaxMaybe[A]] = Tag.subst(arb[Maybe[A]])

  implicit def EitherLeftProjectionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Either.LeftProjection[A, B]] = Functor[Arbitrary].map(arb[Either[A, B]])(_.left)

  implicit def EitherRightProjectionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Either.RightProjection[A, B]] = Functor[Arbitrary].map(arb[Either[A, B]])(_.right)

  implicit def EitherFirstLeftProjectionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Either.LeftProjection[A, B] @@ First] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.left))

  implicit def EitherFirstRightProjectionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Either.RightProjection[A, B] @@ First] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.right))

  implicit def EitherLastLeftProjectionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Either.LeftProjection[A, B] @@ Last] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.left))

  implicit def EitherLastRightProjectionArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[Either.RightProjection[A, B] @@ Last] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.right))

  implicit def ArraySeqArbitrary[A: Arbitrary]: Arbitrary[ArraySeq[A]] = Functor[Arbitrary].map(arb[List[A]])(x => ArraySeq(x: _*))

  import FingerTree._

  implicit def FingerArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Finger[V, A]] = Arbitrary(oneOf(
    arbitrary[A].map(one(_): Finger[V, A]),
    ^(arbitrary[A], arbitrary[A])(two(_, _): Finger[V, A]),
    ^^(arbitrary[A], arbitrary[A], arbitrary[A])(three(_, _, _): Finger[V, A]),
    ^^^(arbitrary[A], arbitrary[A], arbitrary[A], arbitrary[A])(four(_, _, _, _): Finger[V, A])
  ))

  implicit def NodeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Node[V, A]] = Arbitrary(oneOf(
    ^(arbitrary[A], arbitrary[A])(node2[V, A](_, _)),
    ^^(arbitrary[A], arbitrary[A], arbitrary[A])(node3[V, A](_, _, _))
  ))

  implicit def FingerTreeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[FingerTree[V, A]] = Arbitrary {
    def fingerTree[A](n: Int)(implicit a1: Arbitrary[A], measure1: Reducer[A, V]): Gen[FingerTree[V, A]] = n match {
      case 0 => empty[V, A]
      case 1 => arbitrary[A].map(single[V, A](_))
      case n => {
        val nextSize = n.abs / 2
        ^^(FingerArbitrary[V, A].arbitrary,
          fingerTree[Node[V, A]](nextSize)(NodeArbitrary[V, A], implicitly),
          FingerArbitrary[V, A].arbitrary
        )(deep[V, A](_, _, _))
      }
    }
    Gen.sized(fingerTree[A] _)
  }

  implicit def IndSeqArbibrary[A: Arbitrary]: Arbitrary[IndSeq[A]] = Functor[Arbitrary].map(arb[List[A]])(IndSeq.fromSeq)

  import java.util.concurrent.Callable

  implicit def CallableArbitrary[A: Arbitrary]: Arbitrary[Callable[A]] = Functor[Arbitrary].map(arb[A])((x: A) => Applicative[Callable].point(x))

  import Zipper._
  implicit def ZipperArbitrary[A: Arbitrary]: Arbitrary[Zipper[A]] =
    Apply[Arbitrary].apply3[Stream[A], A, Stream[A], Zipper[A]](arb[Stream[A]], arb[A], arb[Stream[A]])(zipper[A](_, _, _))

  implicit def KleisliArbitrary[M[_], A, B](implicit a: Arbitrary[A => M[B]]): Arbitrary[Kleisli[M, A, B]] =
    Functor[Arbitrary].map(a)(Kleisli[M, A, B](_))

  implicit def CokleisliArbitrary[M[_], A, B](implicit a: Arbitrary[M[A] => B]): Arbitrary[Cokleisli[M, A, B]] =
    Functor[Arbitrary].map(a)(Cokleisli[M, A, B](_))

  implicit def CoproductArbitrary[F[_], G[_], A](implicit a: Arbitrary[F[A] \/ G[A]]): Arbitrary[Coproduct[F, G, A]] =
    Functor[Arbitrary].map(a)(Coproduct(_))

  implicit def writerTArb[F[_], W, A](implicit A: Arbitrary[F[(W, A)]]): Arbitrary[WriterT[F, W, A]] =
    Functor[Arbitrary].map(A)(WriterT[F, W, A](_))

  implicit def unwriterTArb[F[_], U, A](implicit A: Arbitrary[F[(U, A)]]): Arbitrary[UnwriterT[F, U, A]] =
    Functor[Arbitrary].map(A)(UnwriterT[F, U, A](_))

  implicit def optionTArb[F[_], A](implicit A: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Functor[Arbitrary].map(A)(OptionT[F, A](_))

  implicit def maybeTArb[F[_], A](implicit A: Arbitrary[F[Maybe[A]]]): Arbitrary[MaybeT[F, A]] =
    Functor[Arbitrary].map(A)(MaybeT[F, A](_))

  implicit def lazyOptionArb[F[_], A](implicit A: Arbitrary[Option[A]]): Arbitrary[LazyOption[A]] =
    Functor[Arbitrary].map(A)(LazyOption.fromOption[A](_))

  implicit def lazyOptionTArb[F[_], A](implicit A: Arbitrary[F[LazyOption[A]]]): Arbitrary[LazyOptionT[F, A]] =
    Functor[Arbitrary].map(A)(LazyOptionT[F, A](_))

  implicit def lazyEitherArb[F[_], A: Arbitrary, B: Arbitrary]: Arbitrary[LazyEither[A, B]] =
    Functor[Arbitrary].map(arb[Either[A, B]]) {
      case Left(a)  => LazyEither.lazyLeft(a)
      case Right(b) => LazyEither.lazyRight(b)
    }

  implicit def lazyEitherTArb[F[_], A, B](implicit A: Arbitrary[F[LazyEither[A, B]]]): Arbitrary[LazyEitherT[F, A, B]] =
    Functor[Arbitrary].map(A)(LazyEitherT[F, A, B](_))

  // backwards compatibility
  def stateTArb[F[+_], S, A](implicit A: Arbitrary[S => F[(S, A)]], F: Monad[F]): Arbitrary[StateT[F, S, A]] =
    indexedStateTArb[F, S, S, A](A, F)

  implicit def indexedReaderWriterStateTArb[F[_], R, W, S1, S2, A](implicit A: Arbitrary[(R, S1) => F[(W, A, S2)]]): Arbitrary[IndexedReaderWriterStateT[F, R, W, S1, S2, A]] =
    Functor[Arbitrary].map(A)(IndexedReaderWriterStateT[F, R, W, S1, S2, A](_))

  implicit def tracedTArb[W[_], A, B](implicit A: Arbitrary[W[A => B]]): Arbitrary[TracedT[W, A, B]] =
    Functor[Arbitrary].map(A)(TracedT(_))

  implicit def indexedContsTArb[W[_], M[_], R, O, A](implicit A: Arbitrary[W[A => M[O]] => M[R]]): Arbitrary[IndexedContsT[W, M, R, O, A]] =
    Functor[Arbitrary].map(A)(IndexedContsT(_))

  implicit def indexedStateTArb[F[_], S1, S2, A](implicit A: Arbitrary[S1 => F[(S2, A)]], F: Monad[F]): Arbitrary[IndexedStateT[F, S1, S2, A]] =
    Functor[Arbitrary].map(A)(IndexedStateT[F, S1, S2, A](_))

  implicit def eitherTArb[F[_], A, B](implicit A: Arbitrary[F[A \/ B]]): Arbitrary[EitherT[F, A, B]] =
      Functor[Arbitrary].map(A)(EitherT[F, A, B](_))

  implicit def theseTArb[F[_], A, B](implicit A: Arbitrary[F[A \&/ B]]): Arbitrary[TheseT[F, A, B]] =
      Functor[Arbitrary].map(A)(TheseT[F, A, B](_))

  implicit def constArbitrary[A: Arbitrary, B]: Arbitrary[Const[A, B]] =
    Functor[Arbitrary].map(arb[A])(Const(_))

  implicit def dlistArbitrary[A](implicit A: Arbitrary[List[A]]) = Functor[Arbitrary].map(A)(as => DList(as : _*))

  implicit def ilistArbitrary[A](implicit A: Arbitrary[List[A]]) = Functor[Arbitrary].map(A)(IList.fromList)

  implicit def dequeueArbitrary[A](implicit A: Arbitrary[List[A]]) = Functor[Arbitrary].map(A)(Dequeue.apply)

  implicit def lazyTuple2Arbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[LazyTuple2[A, B]] =
    Applicative[Arbitrary].apply2(arb[A], arb[B])(LazyTuple2(_, _))

  implicit def lazyTuple3Arbitrary[A: Arbitrary, B: Arbitrary, C: Arbitrary]: Arbitrary[LazyTuple3[A, B, C]] =
    Applicative[Arbitrary].apply3(arb[A], arb[B], arb[C])(LazyTuple3(_, _, _))

  implicit def lazyTuple4Arbitrary[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]: Arbitrary[LazyTuple4[A, B, C, D]] =
    Applicative[Arbitrary].apply4(arb[A], arb[B], arb[C], arb[D])(LazyTuple4(_, _, _, _))

  implicit def heapArbitrary[A](implicit O: Order[A], A: Arbitrary[List[A]]) = {
    Functor[Arbitrary].map(A)(as => Heap.fromData(as))
  }

  // backwards compatibility
  def storeTArb[F[+_], A, B](implicit A: Arbitrary[(F[A => B], A)]): Arbitrary[StoreT[F, A, B]] = indexedStoreTArb[F, A, A, B](A)

  implicit def indexedStoreTArb[F[_], I, A, B](implicit A: Arbitrary[(F[A => B], I)]): Arbitrary[IndexedStoreT[F, I, A, B]] = Functor[Arbitrary].map(A)(IndexedStoreT[F, I, A, B](_))

  implicit def listTArb[F[_], A](implicit FA: Arbitrary[F[List[A]]], F: Applicative[F]): Arbitrary[ListT[F, A]] = Functor[Arbitrary].map(FA)(ListT.fromList(_))

  implicit def streamTArb[F[_], A](implicit FA: Arbitrary[F[Stream[A]]], F: Applicative[F]): Arbitrary[StreamT[F, A]] = Functor[Arbitrary].map(FA)(StreamT.fromStream(_))

  // workaround bug in Scalacheck 1.8-SNAPSHOT.
  private def arbDouble: Arbitrary[Double] = Arbitrary { Gen.oneOf(posNum[Double], negNum[Double])}

  implicit def CaseInsensitiveArbitrary[A](implicit A0: Arbitrary[A], A1: FoldCase[A]): Arbitrary[CaseInsensitive[A]] =
    Functor[Arbitrary].map(A0)(CaseInsensitive(_))

  implicit def dievArbitrary[A](implicit A: Arbitrary[List[A]], E: Enum[A]): Arbitrary[Diev[A]] = Functor[Arbitrary].map(A)(_.grouped(2).foldLeft(Diev.empty[A]){(working, possiblePair) =>
    possiblePair match {
      case first :: second :: Nil => working + ((first, second))
      case value :: Nil => working
      case _ => sys.error("Unexpected amount of items in paired list.")
    }
  })

  implicit def iterateeInputArbitrary[A: Arbitrary]: Arbitrary[scalaz.iteratee.Input[A]] = {
    import scalaz.iteratee.Input._
    Arbitrary(Gen.oneOf(
      Gen.const(emptyInput[A]),
      Gen.const(eofInput[A]),
      arbitrary[A].map(e => elInput(e))
    ))
  }

}
