package scalaz
package scalacheck

import java.math.BigInteger
import org.scalacheck.{Gen, Arbitrary}
import collection.mutable.ArraySeq

/**
 * Instances of {@link scalacheck.Arbitrary} for many types in Scalaz.
 */
object ScalazArbitrary {
  import Scalaz._
  import Tags._
  import Arbitrary._
  import Gen._
  import ScalaCheckBinding._

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def EphemeralStreamArbitrary[A : Arbitrary] =
    Functor[Arbitrary].map(arb[Stream[A]])(EphemeralStream.fromStream[A](_))

  implicit def ImmutableArrayArbitrary[A : Arbitrary : ClassManifest] =
    Functor[Arbitrary].map(arbArray[A])(ImmutableArray.fromArray[A](_))

  implicit def ValueArbitrary[A](implicit fa: Arbitrary[A]): Arbitrary[Value[A]] = Functor[Arbitrary].map(fa)(a => Value(a))
  implicit def NameArbitrary[A](implicit fa: Arbitrary[A]): Arbitrary[Name[A]] = Functor[Arbitrary].map(fa)(a => Name(a))
  implicit def NeedArbitrary[A](implicit fa: Arbitrary[A]): Arbitrary[Need[A]] = Functor[Arbitrary].map(fa)(a => Need(a))

  implicit def UnitArbitrary: Arbitrary[Unit] = Arbitrary(value(()))

  implicit def AlphaArbitrary: Arbitrary[Alpha] = Arbitrary(oneOf(Alpha.alphas))

  implicit def BooleanConjunctionArbitrary: Arbitrary[Boolean @@ Conjunction] = Functor[Arbitrary].map(arb[Boolean])(_.conjunction)

  implicit def arbBigInt: Arbitrary[BigInt] = Apply[Arbitrary].apply2[Int, Int, BigInt](arb[Int], arb[Int])(_ + _)

  implicit def arbBigInteger: Arbitrary[BigInteger] = Functor[Arbitrary].map(arb[BigInt])(_.bigInteger)

  implicit def BigIntegerMultiplicationArbitrary: Arbitrary[BigInteger @@ Multiplication] =
    Tag.subst[BigInteger, Arbitrary, Multiplication](arb[BigInteger])

  implicit def BigIntMultiplicationArbitrary: Arbitrary[BigInt @@ Multiplication] = Tag.subst(arb[BigInt])

  implicit def ByteMultiplicationArbitrary: Arbitrary[Byte @@ Multiplication] = Tag.subst(arb[Byte])

  implicit def CharMultiplicationArbitrary: Arbitrary[Char @@ Multiplication] = Tag.subst(arb[Char])

  implicit def ShortMultiplicationArbitrary: Arbitrary[Short @@ Multiplication] = Tag.subst(arb[Short])

  implicit def IntMultiplicationArbitrary: Arbitrary[Int @@ Multiplication] = Tag.subst(arb[Int])

  implicit def LongMultiplicationArbitrary: Arbitrary[Long @@ Multiplication] = Tag.subst(arb[Long])

  implicit def FloatMultiplicationArbitrary: Arbitrary[Float @@ Multiplication] = Tag.subst(arb[Float])

  implicit def DoubleMultiplicationArbitrary: Arbitrary[Double @@ Multiplication] = Tag.subst(arb[Double])

  implicit def DigitArbitrary: Arbitrary[Digit] = Arbitrary(oneOf(Digit.digits))

  import NonEmptyList._
  implicit def NonEmptyListArbitrary[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Apply[Arbitrary].apply2[A, List[A], NonEmptyList[A]](arb[A], arb[List[A]])(nel(_, _))

  implicit def OneAndArbitrary[F[_], A](implicit A: Arbitrary[A], FA: Arbitrary[F[A]]
                                        ): Arbitrary[OneAnd[F, A]] =
    Apply[Arbitrary].apply2(arb[A], arb[F[A]])(OneAnd.apply)

  implicit def OneOrArbitrary[F[_], A](implicit A: Arbitrary[A], FA: Arbitrary[F[A]]): Arbitrary[OneOr[F, A]] =
    Functor[Arbitrary].map(arb[F[A] \/ A])(OneOr(_))

  import scalaz.Ordering._
  implicit def OrderingArbitrary: Arbitrary[Ordering] = Arbitrary(oneOf(LT, EQ, GT))

  implicit def TreeArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary {
  import scalaz.Tree._
    def tree(n: Int): Gen[Tree[A]] = n match {
      case 0 => arbitrary[A] map (leaf(_))
      case _ => {
        val nextSize = n.abs / 2
        Apply[Gen].apply2(arbitrary[A], resize(n, containerOf[Stream, Tree[A]](Arbitrary(tree(nextSize)).arbitrary)))(node(_, _))
      }
    }
    Gen.sized(tree _)
  }

  implicit def IterableArbitrary[A] (implicit a: Arbitrary[A]): Arbitrary[Iterable[A]] =
      Apply[Arbitrary].apply2[A, List[A], Iterable[A]](arb[A], arb[List[A]])((a, list) => a :: list)

  implicit def TreeLocArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[TreeLoc[A]] =
    Functor[Arbitrary].map(arb[Tree[A]])((t: Tree[A]) => t.loc)

  implicit def DisjunctionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[A \/ B] =
    Functor[Arbitrary].map(arb[Either[A, B]]) {
      case Left(a) => \/.left(a)
      case Right(b) => \/.right(b)
    }

  implicit def ValidationArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Validation[A, B]] =
    Functor[Arbitrary].map(arb[A \/ B])(_.validation)

//  implicit def ZipStreamArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ZipStream[A]] = arb[Stream[A]] ∘ ((s: Stream[A]) => s.ʐ)

  implicit def Tuple1Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Tuple1[A]] = Functor[Arbitrary].map(arb[A])(Tuple1(_))

  implicit def Function0Arbitrary[A](implicit a: Arbitrary[A]): Arbitrary[() => A] = Functor[Arbitrary].map(arb[A])(() => _)

  implicit def FirstOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Option[A] @@ First] = Functor[Arbitrary].map(arb[Option[A]])(_.first)

  implicit def LastOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Option[A] @@ Last] = Functor[Arbitrary].map(arb[Option[A]])(_.last)

  implicit def MinOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[MinOption[A]] = Tag.subst(arb[Option[A]])

  implicit def MaxOptionArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[MaxOption[A]] = Tag.subst(arb[Option[A]])

  implicit def EitherLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B]] = Functor[Arbitrary].map(arb[Either[A, B]])(_.left)

  implicit def EitherRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B]] = Functor[Arbitrary].map(arb[Either[A, B]])(_.right)

  implicit def EitherFirstLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B] @@ First] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.left))

  implicit def EitherFirstRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B] @@ First] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.right))

  implicit def EitherLastLeftProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.LeftProjection[A, B] @@ Last] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.left))

  implicit def EitherLastRightProjectionArbitrary[A, B](implicit a: Arbitrary[A], b: Arbitrary[B]): Arbitrary[Either.RightProjection[A, B] @@ Last] = Functor[Arbitrary].map(arb[Either[A, B]])(x => Tag(x.right))

  implicit def ArraySeqArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ArraySeq[A]] = Functor[Arbitrary].map(arb[List[A]])(x => ArraySeq(x: _*))

  import FingerTree._

  implicit def FingerArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Finger[V, A]] = Arbitrary(oneOf(
    arbitrary[A].map(one(_): Finger[V, A]),
    arbitrary[A].map2(arbitrary[A])(two(_, _): Finger[V, A]),
    arbitrary[A].map3(arbitrary[A], arbitrary[A])(three(_, _, _): Finger[V, A]),
    arbitrary[A].map4(arbitrary[A], arbitrary[A], arbitrary[A])(four(_, _, _, _): Finger[V, A])
  ))

  implicit def NodeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[Node[V, A]] = Arbitrary(oneOf(
    arbitrary[A].map2(arbitrary[A])((a, b) => node2[V, A](a, b)),
    arbitrary[A].map3(arbitrary[A], arbitrary[A])((a, b, c) => node3[V, A](a, b, c))
  ))

  implicit def FingerTreeArbitrary[V, A](implicit a: Arbitrary[A], measure: Reducer[A, V]): Arbitrary[FingerTree[V, A]] = Arbitrary {
    def fingerTree[A](n: Int)(implicit a1: Arbitrary[A], measure1: Reducer[A, V]): Gen[FingerTree[V, A]] = n match {
      case 0 => empty[V, A]
      case 1 => arbitrary[A].map(single[V, A](_))
      case n => {
        val nextSize = n.abs / 2
        arbitrary[Finger[V, A]].map3(fingerTree[Node[V, A]](nextSize), arbitrary[Finger[V, A]])(deep[V, A](_, _, _))
      }
    }
    Gen.sized(fingerTree[A] _)
  }

  implicit def RopeArbitrary[A : Arbitrary : ClassManifest]: Arbitrary[Rope[A]] =
    Functor[Arbitrary].map(FingerTreeArbitrary(ImmutableArrayArbitrary[A], Rope.sizer[A]))(Rope[A](_))

  import java.util.concurrent.Callable

  implicit def CallableArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Callable[A]] = Functor[Arbitrary].map(arb[A])((x: A) => Applicative[Callable].point(x))

  import scalaz.concurrent.Promise

  implicit def PromiseArbitrary[A](implicit a: Arbitrary[A], s: concurrent.Strategy): Arbitrary[Promise[A]] = Functor[Arbitrary].map(arb[A])((x: A) => Promise(x))

  import scalaz.concurrent.Future
  implicit def FutureArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Future[A]] =
    Arbitrary(arbitrary[A] map ((x: A) => Future.now(x)))

  import Zipper._
  implicit def ZipperArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Zipper[A]] =
    Apply[Arbitrary].apply3[Stream[A], A, Stream[A], Zipper[A]](arb[Stream[A]], arb[A], arb[Stream[A]])(zipper[A](_, _, _))

  implicit def KleisliArbitrary[M[+_], A, B](implicit a: Arbitrary[A => M[B]]): Arbitrary[Kleisli[M, A, B]] =
    Functor[Arbitrary].map(a)(Kleisli[M, A, B](_))

  implicit def writerTArb[F[+_], W, A](implicit A: Arbitrary[F[(W, A)]]): Arbitrary[WriterT[F, W, A]] =
    Functor[Arbitrary].map(A)(WriterT[F, W, A](_))

  implicit def unwriterTArb[F[+_], U, A](implicit A: Arbitrary[F[(U, A)]]): Arbitrary[UnwriterT[F, U, A]] =
    Functor[Arbitrary].map(A)(UnwriterT[F, U, A](_))

  implicit def optionTArb[F[+_], A](implicit A: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Functor[Arbitrary].map(A)(OptionT[F, A](_))

  implicit def lazyOptionArb[F[_], A](implicit A: Arbitrary[Option[A]]): Arbitrary[LazyOption[A]] =
    Functor[Arbitrary].map(A)(LazyOption.fromOption[A](_))

  implicit def lazyOptionTArb[F[+_], A](implicit A: Arbitrary[F[LazyOption[A]]]): Arbitrary[LazyOptionT[F, A]] =
    Functor[Arbitrary].map(A)(LazyOptionT[F, A](_))

  implicit def lazyEitherArb[F[_], A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[LazyEither[A, B]] =
    Functor[Arbitrary].map(arb[Either[A, B]]) {
      case Left(a)  => LazyEither.lazyLeft(a)
      case Right(b) => LazyEither.lazyRight(b)
    }

  implicit def lazyEitherTArb[F[+_], A, B](implicit A: Arbitrary[F[LazyEither[A, B]]]): Arbitrary[LazyEitherT[F, A, B]] =
    Functor[Arbitrary].map(A)(LazyEitherT[F, A, B](_))

  // backwards compatibility
  def stateTArb[F[+_], S, A](implicit A: Arbitrary[S => F[(S, A)]]): Arbitrary[StateT[F, S, A]] =
    indexedStateTArb[F, S, S, A](A)

  implicit def indexedStateTArb[F[+_], S1, S2, A](implicit A: Arbitrary[S1 => F[(S2, A)]]): Arbitrary[IndexedStateT[F, S1, S2, A]] =
    Functor[Arbitrary].map(A)(IndexedStateT[F, S1, S2, A](_))

  implicit def eitherTArb[F[+_], A, B](implicit A: Arbitrary[F[A \/ B]]): Arbitrary[EitherT[F, A, B]] =
      Functor[Arbitrary].map(A)(EitherT[F, A, B](_))

  implicit def dlistArbitrary[A](implicit A: Arbitrary[List[A]]) = Functor[Arbitrary].map(A)(as => DList(as : _*))

  implicit def lazyTuple2Arbitrary[A, B](implicit A: Arbitrary[A], B: Arbitrary[B]): Arbitrary[LazyTuple2[A, B]] =
    Applicative[Arbitrary].apply2(A, B)(LazyTuple2(_, _))

  implicit def lazyTuple3Arbitrary[A, B, C](implicit A: Arbitrary[A], B: Arbitrary[B], C: Arbitrary[C]): Arbitrary[LazyTuple3[A, B, C]] =
    Applicative[Arbitrary].apply3(A, B, C)(LazyTuple3(_, _, _))

  implicit def lazyTuple4Arbitrary[A, B, C, D](implicit A: Arbitrary[A], B: Arbitrary[B], C: Arbitrary[C], D: Arbitrary[D]): Arbitrary[LazyTuple4[A, B, C, D]] =
    Applicative[Arbitrary].apply4(A, B, C, D)(LazyTuple4(_, _, _, _))

  implicit def heapArbitrary[A](implicit O: Order[A], A: Arbitrary[List[A]]) = {
    Functor[Arbitrary].map(A)(as => Heap.fromData(as))
  }

  implicit def insertionMapArbitrary[A, B](implicit A: Arbitrary[List[(A, B)]]): Arbitrary[InsertionMap[A, B]] = {
    Functor[Arbitrary].map(A)(as => InsertionMap(as: _*))
  }

  @deprecated("BKTree is deprecated", "7.0.1")
  implicit def bkTreeArbitrary[A](implicit A: MetricSpace[A], arb: Arbitrary[List[A]]): Arbitrary[BKTree[A]] =
    Functor[Arbitrary].map(arb)(as => BKTree[A](as: _*))

  // backwards compatability
  def storeTArb[F[+_], A, B](implicit A: Arbitrary[(F[A => B], A)]): Arbitrary[StoreT[F, A, B]] = indexedStoreTArb[F, A, A, B](A)

  implicit def indexedStoreTArb[F[+_], I, A, B](implicit A: Arbitrary[(F[A => B], I)]): Arbitrary[IndexedStoreT[F, I, A, B]] = Functor[Arbitrary].map(A)(IndexedStoreT[F, I, A, B](_))

  implicit def listTArb[F[+_], A](implicit FA: Arbitrary[F[List[A]]], F: Applicative[F]): Arbitrary[ListT[F, A]] = Functor[Arbitrary].map(FA)(ListT.fromList(_))

  implicit def streamTArb[F[+_], A](implicit FA: Arbitrary[F[Stream[A]]], F: Applicative[F]): Arbitrary[StreamT[F, A]] = Functor[Arbitrary].map(FA)(StreamT.fromStream(_))

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

  import scalaz.xml._
  import scalaz.xml.cursor._
  import scalaz.xml.Xml.{Line, Str}

  /** @see [[https://groups.google.com/d/topic/scalacheck/O7e7k5JZKKI]] */
  def smallListArb[A](implicit A: Arbitrary[A]): Arbitrary[List[A]] =
    Arbitrary(choose(0, 5).flatMap{ n =>
      listOfN(n, arbitrary[A])
    })

  implicit val qnameArbitrary: Arbitrary[QName] =
    ^^(arb[Str],arb[Option[Str]],arb[Option[Str]])(QName.qname)

  implicit val attrArbitrary: Arbitrary[Attr] =
    ^(arb[QName],arb[Str])(Attr.attr)

  implicit val cdataKindArbitrary: Arbitrary[CDataKind] = {
    import CDataKind._
    Arbitrary(oneOf(cdataText, cdataVerbatim, cdataRaw))
  }

  implicit val cdataArbitrary: Arbitrary[CData] =
    ^^(arb[CDataKind], arb[Str], arb[Option[Line]])(CData.cdata)

  implicit val elementArbitrary: Arbitrary[Element] =
    ^^^(arb[QName], arb[List[Attr]], smallListArb[Content], arb[Option[Line]])(Element.element)

  implicit val contentArbitrary: Arbitrary[Content] = {
    import Content._
    Arbitrary(oneOf(
      arbitrary[Element].map(elem),
      arbitrary[CData].map(text),
      arbitrary[Str].map(cref),
      arbitrary[Str].map(comment)
    ))
  }

  implicit val nsInfoArbitrary: Arbitrary[NSInfo] =
    ^(arb[List[(Str, Str)]],arb[Option[Str]])(NSInfo.nsInfo)

  implicit val tokenArbitrary: Arbitrary[Token] = {
    import Token._
    Arbitrary(oneOf(
      ^^^(arbitrary[CData.Line], arbitrary[QName], arbitrary[List[Attr]], arbitrary[Boolean])(startToken),
      ^(arbitrary[CData.Line], arbitrary[QName])(endToken),
      arbitrary[Str].map(crefToken),
      arbitrary[CData].map(textToken),
      arbitrary[Str].map(commentToken)
    ))
  }

  implicit val tagArbitrary: Arbitrary[Tag] =
    ^^(arb[QName], arb[List[Attr]], arb[Option[Line]])(Tag.tag)

  implicit val cursorArbitrary: Arbitrary[Cursor] =
    ^^^(arb[Content], smallListArb[Content], smallListArb[Content], arb[Cursor.Path])(Cursor.cursor)

  implicit def predicateArbitrary[A](implicit P: Arbitrary[A => Boolean]): Arbitrary[Predicate[A]] =
    ^(P, arb[Option[List[Char]]])(Predicate.predicate)

  implicit val opArbitrary: Arbitrary[Op] = {
    import Op._
    Arbitrary(oneOf(
      arbitrary[History].map(choiceSucceedOp),
      arbitrary[History].map(h => choiceSwitchOp(h,h)),
      arbitrary[Predicate.CPredicate].map(findLeftOp),
      arbitrary[Predicate.CPredicate].map(findRightOp),
      arbitrary[Predicate.CPredicate].map(findChildOp),
      arbitrary[Predicate.CPredicate].map(findRecOp),
      arbitrary[Int].map(n => nthChildOp(n)),
      ^(arbitrary[Cursor => Cursor], arbitrary[OpDescription])(succeedingOp),
      ^(arbitrary[Cursor => Option[Cursor]], arbitrary[OpDescription])(genericOp),
      failedComposeOp, leftOp, rightOp, firstChildOp, lastChildOp, remove, removeLeftOp, removeRightOp, parentOp, rootOp, nextDepthFirstOp
    ))
  }

  implicit val historyArbitrary: Arbitrary[History] =
    smallListArb[Op].map(_.foldRight(History.history)(_ +: _))

  implicit val hCursorArbitrary: Arbitrary[HCursor] =
    ^(arb[History], arb[Option[Cursor]])(HCursor.hcursor)

}
