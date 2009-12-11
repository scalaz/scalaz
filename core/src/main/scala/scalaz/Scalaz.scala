package scalaz

import concurrent._
import geo._

object Scalaz extends ScalazLow
    with    Actors
    with    Alphas
    with    Applys
    with    ArrayBytes
    with    Azimuths
    with    Bearings
    with    BigIntegerMultiplications
    with    BigIntegers
    with    BigIntMultiplications
    with    BigInts
    with    BKTrees
    with    Booleans
    with    BooleanConjunctions
    with    ByteMultiplications
    with    Bytes
    with    CharMultiplications
    with    Chars
    with    CharSets
    with    Cokleislis
    with    Coords
    with    Digits
    with    DLists
    with    DoubleWs
    with    Duals
    with    Effects
    with    Elevations
    with    Ellipsoids
    with    Emptys
    with    Endos
    with    Enumerations
    with    Equals
    with    FirstOptions
    with    Function0s
    with    Function1s
    with    Function2s
    with    GeodeticCurves
    with    Kleislis
    with    Identitys
    with    InputStreams
    with    IntMultiplications
    with    Ints
    with    Iterables
    with    LastOptions
    with    Latitudes
    with    Lists
    with    Longitudes
    with    LongMultiplications
    with    Longs
    with    MAs
    with    MetricSpaces
    with    Memos
    with    NonEmptyLists
    with    Options
    with    Orders
    with    Positions
    with    Promises
    with    Radianss
    with    Semigroups
    with    ShortMultiplications
    with    Shorts
    with    Shows
    with    States
    with    Streams
    with    Strings
    with    Trees
    with    TreeLocs
    with    Validations
    with    Vectors
    with    Zeros
    with    Zippers
    with    ZipStreams {
  def ⊥ = error("undefined")

  def undefined = ⊥

  type ⊤ = Any

  type ℤ = math.BigInt

  lazy val π = java.lang.Math.PI

  lazy val π2 = π * 2   

  type GArray[A] = collection.mutable.GenericArray[A]

  type ![A <: Applicable, B] = A#Apply[B]

  def ×[A, B] = (a: A) => (b: B) => (a, b)

  def i[F[_], A](implicit i: F[A]): F[A] = i

  // todo move to MAs, once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def Function1FlipMACofunctor[A, R](f: R => A): MACofunctor[PartialApply1Of2[Function1, A]#Flip, R] = maCofunctor[PartialApply1Of2[Function1, A]#Flip, R](f)

  implicit def Function1ApplyMA[A, R](f: A => R): MA[PartialApply1Of2[Function1, A]#Apply, R] = ma[PartialApply1Of2[Function1, A]#Apply, R](f)

  implicit def StateMA[S, A](s: State[S, A]): MA[PartialApply1Of2[State, S]#Apply, A] = ma[PartialApply1Of2[State, S]#Apply, A](s)

  implicit def ValidationMA[A, E](v: Validation[E, A]): MA[PartialApply1Of2[Validation, E]#Apply, A] = ma[PartialApply1Of2[Validation, E]#Apply, A](v)

  // todo not working, see ImplicitConversionsTest
  // implicit def ValidationFailureMA[A, E](f: FailProjection[E, A]): MA[PartialApply1Of2[FailProjection, A]#Flip, E] = ma[PartialApply1Of2[FailProjection, A]#Flip, E](f)

  // todo Add TupleN, FunctionN, Left, Right, Map.Entry, and tests in ImplicitConversionsTest.

  // Seq[A] implements Function1[Int, A]. Without this, Function1FlipMA would be used.
  implicit def SeqMA[M[_] <: Seq[_], A](l: M[A]): MA[M, A] = ma[M, A](l)
}
