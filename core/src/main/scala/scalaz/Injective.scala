package scalaz

import collection.immutable.IndexedSeq

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
  implicit def DisjunctionInjective     = Injective2[\/]()
  implicit def EitherInjective     = Injective2[Either]()
  implicit def EitherRightProjectionInjective = Injective2[Either.RightProjection]()
  implicit def EitherLeftProjectionInjective = Injective2[Either.LeftProjection]()
  implicit def FractionalInjective = Injective[Fractional]()
  implicit def Function0Injective  = Injective[Function0]()
  implicit def Function1Injective  = Injective2[Function1]()
  implicit def Function2Injective  = Injective3[Function2]()
  implicit def Function3Injective  = Injective4[Function3]()
  implicit def Function4Injective  = Injective5[Function4]()
  implicit def IndexedSeqInjective = Injective[IndexedSeq]()
  implicit def IntegralInjective   = Injective[Integral]()
  implicit def IterableInjective   = Injective[Iterable]()
  implicit def IteratorInjective   = Injective[Iterator]()
  implicit def ListInjective       = Injective[List]()
  implicit def OptionInjective     = Injective[Option]()
  implicit def PartialFunctionInjective = Injective2[PartialFunction]()
  implicit def PartialOrderingInjective = Injective[PartialOrdering]()
  implicit def Product1Injective   = Injective[Product1]()
  implicit def Product2Injective   = Injective2[Product2]()
  implicit def Product3Injective   = Injective3[Product3]()
  implicit def Product4Injective   = Injective4[Product4]()
  implicit def Product5Injective   = Injective5[Product5]()
  implicit def SetInjective        = Injective[Set]()
  implicit def Tuple2Injective     = Injective2[Tuple2]()
  implicit def Tuple3Injective     = Injective3[Tuple3]()
  implicit def Tuple4Injective     = Injective4[Tuple4]()
  implicit def Tuple5Injective     = Injective5[Tuple5]()
  implicit def ValidationInjective = Injective2[Validation]()
  implicit def StreamInjective     = Injective[Stream]()
}
