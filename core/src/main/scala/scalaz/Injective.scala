package scalaz

import scala.collection.immutable.IndexedSeq

/** Given Injective[Foo]: If type Foo[A] = Foo[B] then A ~ B
  *
  * This represents an assertion that is used by other code that requires this condition.
  *
  */
case class Injective [T[_]]()
case class Injective2[T[_,_]]()
case class Injective3[T[_,_,_]]()
case class Injective4[T[_,_,_,_]]()
case class Injective5[T[_,_,_,_,_]]()

object Injectivity {
  implicit def DisjunctionInjective: Injective2[\/] = Injective2()
  implicit def EitherInjective: Injective2[Either] = Injective2()
  implicit def EitherRightProjectionInjective: Injective2[Either.RightProjection] = Injective2()
  implicit def EitherLeftProjectionInjective: Injective2[Either.LeftProjection] = Injective2()
  implicit def FractionalInjective: Injective[Fractional] = Injective()
  implicit def Function0Injective: Injective[Function0] = Injective()
  implicit def Function1Injective: Injective2[Function] = Injective2()
  implicit def Function2Injective: Injective3[Function2] = Injective3()
  implicit def Function3Injective: Injective4[Function3] = Injective4()
  implicit def Function4Injective: Injective5[Function4] = Injective5()
  implicit def IndexedSeqInjective: Injective[IndexedSeq] = Injective()
  implicit def IntegralInjective: Injective[Integral] = Injective()
  implicit def IterableInjective: Injective[Iterable] = Injective()
  implicit def IteratorInjective: Injective[Iterator] = Injective()
  implicit def ListInjective: Injective[List] = Injective()
  implicit def OptionInjective: Injective[Option] = Injective()
  implicit def PartialFunctionInjective: Injective2[PartialFunction] = Injective2()
  implicit def PartialOrderingInjective: Injective[PartialOrdering] = Injective()
  implicit def Product1Injective: Injective[Product1] = Injective()
  implicit def Product2Injective: Injective2[Product2] = Injective2()
  implicit def Product3Injective: Injective3[Product3] = Injective3()
  implicit def Product4Injective: Injective4[Product4] = Injective4()
  implicit def Product5Injective: Injective5[Product5] = Injective5()
  implicit def SetInjective: Injective[Set] = Injective()
  implicit def Tuple2Injective: Injective2[Tuple2] = Injective2()
  implicit def Tuple3Injective: Injective3[Tuple3] = Injective3()
  implicit def Tuple4Injective: Injective4[Tuple4] = Injective4()
  implicit def Tuple5Injective: Injective5[Tuple5] = Injective5()
  implicit def ValidationInjective: Injective2[Validation] = Injective2()
  implicit def StreamInjective: Injective[Stream] = Injective()
}
