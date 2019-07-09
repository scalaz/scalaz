package scalaz
package scalacheck

import java.math.BigInteger
import org.scalacheck.{Cogen, Gen, Arbitrary}
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
    Cogen[EphemeralStream[A]].contramap(_.toUnsortedStream)

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

  implicit def cogenIndexedContsT[W[_], R, O, M[_], A](implicit F: Cogen[W[A => M[O]] => M[R]]): Cogen[IndexedContsT[W, R, O, M, A]] =
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

  implicit def cogenEitherT[A, F[_], B](implicit F: Cogen[F[A \/ B]]): Cogen[EitherT[A, F, B]] =
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

  implicit def cogenIndexedReaderWriterStateT[R, W, S1, S2, F[_]: Monad, A](implicit F: Cogen[(R, S1) => F[(W, A, S2)]]): Cogen[IndexedReaderWriterStateT[R, W, S1, S2, F, A]] =
    F.contramap(_.run)

  implicit def cogenIndexedStateT[F[_]: Monad, S1, S2, A](implicit F: Cogen[S1 => F[(S2, A)]]): Cogen[IndexedStateT[S1, S2, F, A]] =
    F.contramap(s => s.apply(_))

  implicit def cogenWriterT[A, F[_], B](implicit F: Cogen[F[(A, B)]]): Cogen[WriterT[A, F, B]] =
    F.contramap(_.run)

  implicit def cogenUnwriterT[F[_], A, B](implicit F: Cogen[F[(A, B)]]): Cogen[UnwriterT[F, A, B]] =
    F.contramap(_.run)

  implicit def cogenTree[A: Cogen]: Cogen[Tree[A]] =
    Cogen[List[A]].contramap(_.toList)

  implicit def cogenTreeLoc[A: Cogen]: Cogen[TreeLoc[A]] =
    Divide[Cogen].dividing4(Function.unlift(TreeLoc.unapply))

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

  implicit def cogenAp[F[_], A](implicit F: Cogen[F[A]]): Cogen[Ap[F, A]] =
    F.contramap(_.f)

  implicit def cogenTannen[F[_], G[_, _], A, B](implicit F: Cogen[F[G[A, B]]]): Cogen[Tannen[F, G, A, B]] =
    F.contramap(_.f)

  implicit def tannenArbitrary[F[_], G[_, _], A, B](implicit F: Arbitrary[F[G[A, B]]]): Arbitrary[Tannen[F, G, A, B]] =
    Functor[Arbitrary].map(F)(Tannen[F, G, A, B](_))

  implicit def apArbitrary[F[_], A](implicit F: Arbitrary[F[A]]): Arbitrary[Ap[F, A]] =
    Functor[Arbitrary].map(F)(Ap[F, A](_))

  implicit def cogenAlter[F[_], A](implicit F: Cogen[F[A]]): Cogen[Alter[F, A]] =
    F.contramap(_.f)

  implicit def alterArbitrary[F[_], A](implicit F: Arbitrary[F[A]]): Arbitrary[Alter[F, A]] =
    Functor[Arbitrary].map(F)(Alter[F, A](_))

  implicit def monoidCoproductArbitrary[M: Arbitrary, N: Arbitrary]: Arbitrary[M :+: N] =
    Functor[Arbitrary].map(arb[Vector[M \/ N]])(new :+:(_))

  /** @since 7.0.3 */
  implicit def theseArb[A: Arbitrary, B: Arbitrary]: Arbitrary[A \&/ B] =
    Arbitrary(Gen.oneOf(
      ^(arbitrary[A], arbitrary[B])(\&/.Both(_, _)),
      arbitrary[A].map(\&/.This[A, B](_)),
      arbitrary[B].map(\&/.That[A, B](_))
    ))

  implicit def endoArb[A](implicit A: Arbitrary[A => A]): Arbitrary[Endo[A]] =
    Functor[Arbitrary].map(A)(Endo.endo)

  implicit def endoByNameArb[A](implicit A: Arbitrary[A => A]): Arbitrary[EndoByName[A]] =
    Functor[Arbitrary].map(A)(f => Endo.endoByName(f(_)))

  implicit def endomorphicArbitrary[F[_, _], A](implicit F: Arbitrary[F[A, A]]): Arbitrary[Endomorphic[F, A]] =
    Functor[Arbitrary].map(F)(Endomorphic[F, A](_))

  implicit def EphemeralStreamArbitrary[A : Arbitrary]: Arbitrary[EphemeralStream[A]] =
    Functor[Arbitrary].map(arb[Stream[A]])(EphemeralStream.fromStream[A](_))

  implicit def IStreamArbitrary[A : Arbitrary]: Arbitrary[IStream[A]] =
    Functor[Arbitrary].map(arb[Stream[A]])(IStream.fromStream[A](_))

  implicit def CorecursiveListArbitrary[A : Arbitrary]: Arbitrary[CorecursiveList[A]] =
    Functor[Arbitrary].map(arb[Stream[A]])(CorecursiveList.fromStream)

  implicit def ImmutableArrayArbitrary[A : Arbitrary : ClassTag]: Arbitrary[ImmutableArray[A]] =
    Functor[Arbitrary].map(arb[Array[A]])(ImmutableArray.fromArray[A](_))

  implicit def ValueArbitrary[A: Arbitrary]: Arbitrary[Value[A]] = Functor[Arbitrary].map(arb[A])(a => Value(a))
  implicit def NameArbitrary[A: Arbitrary]: Arbitrary[Name[A]] = Functor[Arbitrary].map(arb[A])(a => Name(a))
  implicit def NeedArbitrary[A: Arbitrary]: Arbitrary[Need[A]] = Functor[Arbitrary].map(arb[A])(a => Need(a))

  implicit val UnitArbitrary: Arbitrary[Unit] = Arbitrary(const(()))

  implicit val AlphaArbitrary: Arbitrary[Alpha] = {
    val alphaList = Alpha.alphas.toList
    Arbitrary(oneOf(alphaList))
  }

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

  implicit val IntDualArbitrary: Arbitrary[Int @@ Dual] = Tag.subst(arb[Int])

  implicit val DigitArbitrary: Arbitrary[Digit] = {
    val digitList = Digit.digits.toList
    Arbitrary(oneOf(digitList))
  }

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

  private[this] def withSize[A](size: Int)(f: Int => Gen[A]): Gen[EphemeralStream[A]] = {
    Applicative[Gen].sequence(
      (IList.fill(size)(Gen.choose(1, size max 1))).toEphemeralStream
    ).flatMap { s =>
      val ns = Traverse[EphemeralStream].traverseS(s) { n =>
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
      }.eval(0).flatMap(o => o.cata[EphemeralStream[Int]](EphemeralStream(_), EphemeralStream.emptyEphemeralStream))

      Applicative[Gen].sequence(ns.map(f))
    }
  }

  private[scalaz] def treeGenSized[A: NotNothing](size: Int)(implicit A: Arbitrary[A]): Gen[Tree[A]] =
    size match {
      case n if n <= 1 =>
        A.arbitrary.map(a => Tree.Leaf(a))
      case 2 =>
        arb[(A, A)].arbitrary.map{ case (a1, a2) =>
          Tree.Node(a1, EphemeralStream(Tree.Leaf(a2)))
        }
      case 3 =>
        arb[(A, A, A)].arbitrary.flatMap{ case (a1, a2, a3) =>
          Gen.oneOf(
            Tree.Node(a1, EphemeralStream(Tree.Leaf(a2), Tree.Leaf(a3))),
            Tree.Node(a1, EphemeralStream(Tree.Node(a2, EphemeralStream(Tree.Leaf(a3)))))
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
      Gen.choose(0, (n - 1) max 0).flatMap { x1 =>
        Apply[Gen].tuple3(
          forest(x1), A.arbitrary, forest(n - x1 - 1)
        )
      }
    }

    for{
      a <- Gen.choose(1, size max 1)
      b = size - a
      aa <- Gen.choose(1, a max 1)
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
    Functor[Arbitrary].map(arb[A \/ B])(_.toValidation)

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

  import FingerTree._

  implicit def FingerArbitrary[V: Monoid, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Finger[V, A]] = Arbitrary(oneOf(
    arbitrary[A].map(one(_): Finger[V, A]),
    ^(arbitrary[A], arbitrary[A])(two(_, _): Finger[V, A]),
    ^^(arbitrary[A], arbitrary[A], arbitrary[A])(three(_, _, _): Finger[V, A]),
    ^^^(arbitrary[A], arbitrary[A], arbitrary[A], arbitrary[A])(four(_, _, _, _): Finger[V, A])
  ))

  implicit def NodeArbitrary[V: Monoid, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Node[V, A]] = Arbitrary(oneOf(
    ^(arbitrary[A], arbitrary[A])(node2[V, A](_, _)),
    ^^(arbitrary[A], arbitrary[A], arbitrary[A])(node3[V, A](_, _, _))
  ))

  implicit def FingerTreeArbitrary[V: Monoid, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[FingerTree[V, A]] = Arbitrary {
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

  implicit def writerTArb[W, F[_], A](implicit A: Arbitrary[F[(W, A)]]): Arbitrary[WriterT[W, F, A]] =
    Functor[Arbitrary].map(A)(WriterT[W, F, A](_))

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

  implicit def idTArbitrary[A, F[_]](implicit A: Arbitrary[F[A]]): Arbitrary[IdT[F, A]] =
    Functor[Arbitrary].map(A)(IdT[F, A])

  // backwards compatibility
  def stateTArb[S, F[+_], A](implicit A: Arbitrary[S => F[(S, A)]]): Arbitrary[StateT[S, F, A]] =
    indexedStateTArb[S, S, F, A](A)

  implicit def indexedReaderWriterStateTArb[R, W, S1, S2, F[_], A](implicit A: Arbitrary[(R, S1) => F[(W, A, S2)]]): Arbitrary[IndexedReaderWriterStateT[R, W, S1, S2, F, A]] =
    Functor[Arbitrary].map(A)(IndexedReaderWriterStateT[R, W, S1, S2, F, A](_))

  implicit def tracedTArb[W[_], A, B](implicit A: Arbitrary[W[A => B]]): Arbitrary[TracedT[W, A, B]] =
    Functor[Arbitrary].map(A)(TracedT(_))

  implicit def indexedContsTArb[W[_], R, O, M[_], A](implicit A: Arbitrary[W[A => M[O]] => M[R]]): Arbitrary[IndexedContsT[W, R, O, M, A]] =
    Functor[Arbitrary].map(A)(IndexedContsT(_))

  implicit def indexedStateTArb[S1, S2, F[_], A](implicit A: Arbitrary[S1 => F[(S2, A)]]): Arbitrary[IndexedStateT[S1, S2, F, A]] =
    Functor[Arbitrary].map(A)(IndexedStateT[S1, S2, F, A](_))

  implicit def eitherTArb[A, F[_], B](implicit A: Arbitrary[F[A \/ B]]): Arbitrary[EitherT[A, F, B]] =
      Functor[Arbitrary].map(A)(EitherT[A, F, B](_))

  implicit def theseTArb[F[_], A, B](implicit A: Arbitrary[F[A \&/ B]]): Arbitrary[TheseT[F, A, B]] =
    Functor[Arbitrary].map(A)(TheseT[F, A, B](_))

  implicit def constArbitrary[A: Arbitrary, B]: Arbitrary[Const[A, B]] =
    Functor[Arbitrary].map(arb[A])(Const(_))

  implicit def dlistArbitrary[A](implicit A: Arbitrary[List[A]]): Arbitrary[DList[A]] = Functor[Arbitrary].map(A)(as => DList(as : _*))

  implicit def ilistArbitrary[A](implicit A: Arbitrary[List[A]]): Arbitrary[IList[A]] = Functor[Arbitrary].map(A)(IList.fromList)

  implicit def dequeueArbitrary[A](implicit A: Arbitrary[List[A]]): Arbitrary[Dequeue[A]] = Functor[Arbitrary].map(A)(Dequeue.apply)

  implicit def lazyTuple2Arbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[LazyTuple2[A, B]] =
    Applicative[Arbitrary].apply2(arb[A], arb[B])(LazyTuple2(_, _))

  implicit def lazyTuple3Arbitrary[A: Arbitrary, B: Arbitrary, C: Arbitrary]: Arbitrary[LazyTuple3[A, B, C]] =
    Applicative[Arbitrary].apply3(arb[A], arb[B], arb[C])(LazyTuple3(_, _, _))

  implicit def lazyTuple4Arbitrary[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary]: Arbitrary[LazyTuple4[A, B, C, D]] =
    Applicative[Arbitrary].apply4(arb[A], arb[B], arb[C], arb[D])(LazyTuple4(_, _, _, _))

  implicit def heapArbitrary[A](implicit O: Order[A], A: Arbitrary[List[A]]): Arbitrary[Heap[A]] = {
    Functor[Arbitrary].map(A)(as => Heap.fromData(as))
  }

  // backwards compatibility
  def storeTArb[F[+_], A, B](implicit A: Arbitrary[(F[A => B], A)]): Arbitrary[StoreT[F, A, B]] = indexedStoreTArb[F, A, A, B](A)

  implicit def indexedStoreTArb[F[_], I, A, B](implicit A: Arbitrary[(F[A => B], I)]): Arbitrary[IndexedStoreT[F, I, A, B]] = Functor[Arbitrary].map(A)(IndexedStoreT[F, I, A, B](_))

  implicit def listTArb[F[_]: Applicative, A](implicit FA: Arbitrary[F[IList[A]]]): Arbitrary[ListT[F, A]] = Functor[Arbitrary].map(FA)(ListT.fromIList(_))

  implicit def streamTArb[F[_], A](implicit FA: Arbitrary[F[Stream[A]]], F: Applicative[F]): Arbitrary[StreamT[F, A]] = Functor[Arbitrary].map(FA)(StreamT.fromStream(_))

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
