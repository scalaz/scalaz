package scalaz

import concurrent._

object Scalaz extends ScalazLow
    with    Actors
    with    Alphas
    with    Applys
    with    ArrayBytes
    with    BigIntegers
    with    BigInts
    with    BKTrees
    with    Booleans
    with    BooleanConjunctions
    with    Bytes
    with    Chars
    with    CharSets
    with    Cokleislis
    with    Comps
    with    Digits
    with    DLists
    with    Duals
    with    Effects
    with    Emptys
    with    Endos
    with    Enumerations
    with    Equals
    with    Extras
    with    FirstOptions
    with    Function0s
    with    Function1s
    with    Function2s
    with    Kleislis
    with    Identitys
    with    InputStreams
    with    Ints
    with    Iterables
    with    LastOptions
    with    LazyTuples
    with    Lists
    with    Longs
    with    MAs
    with    MetricSpaces
    with    Memos
    with    Multiplications
    with    NonEmptyLists
    with    Options
    with    Orders
    with    Promises
    with    Reducers
    with    Semigroups 
    with    Shorts
    with    Shows
    with    States
    with    Streams
    with    Strings
    with    Trees
    with    TreeLocs
    with    Validations
    with    Zeros
    with    Zippers
    with    ZipStreams {
  def ⊥ = error("undefined")

  def undefined = ⊥

  type ⊤ = Any

  type ℤ = scala.math.BigInt

  lazy val π = java.lang.Math.PI

  lazy val π2 = π * 2

  type ArraySeq[A] = collection.mutable.ArraySeq[A]

  val ArraySeq = collection.mutable.ArraySeq

  def ×[A, B] = (a: A) => (b: B) => (a, b)

  // todo move these to MAs, once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def EitherLeftMA[X, A](a: Either.LeftProjection[A, X]) = ma[PartialApply1Of2[Either.LeftProjection, X]#Flip, A](a)

  implicit def EitherRightMA[X, A](a: Either.RightProjection[X, A]): MA[PartialApply1Of2[Either.RightProjection, X]#Apply, A] = ma[PartialApply1Of2[Either.RightProjection, X]#Apply, A](a)

  implicit def Function1FlipMACofunctor[A, R](f: R => A): MACofunctor[PartialApply1Of2[Function1, A]#Flip, R] = maCofunctor[PartialApply1Of2[Function1, A]#Flip, R](f)

  implicit def Function1ApplyMA[A, R](f: A => R): MA[PartialApply1Of2[Function1, A]#Apply, R] = ma[PartialApply1Of2[Function1, A]#Apply, R](f)

  implicit def Function2MA[R, S, A](a: (R, S) => A): MA[PartialApply2Of3[Function2, R, S]#Apply, A] = ma[PartialApply2Of3[Function2, R, S]#Apply, A](a)

  implicit def Function3MA[R, S, T, A](a: (R, S, T) => A): MA[PartialApply3Of4[Function3, R, S, T]#Apply, A] = ma[PartialApply3Of4[Function3, R, S, T]#Apply, A](a)

  implicit def Function4MA[R, S, T, U, A](a: (R, S, T, U) => A): MA[PartialApply4Of5[Function4, R, S, T, U]#Apply, A] = ma[PartialApply4Of5[Function4, R, S, T, U]#Apply, A](a)

  implicit def Function5MA[R, S, T, U, V, A](a: (R, S, T, U, V) => A): MA[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A] = ma[PartialApply5Of6[Function5, R, S, T, U, V]#Apply, A](a)

  implicit def Function6MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => A): MA[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A] = ma[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply, A](a)

  implicit def ConstMA[B, A](c: Const[B, A]): MA[PartialApply1Of2[Const, B]#Apply, A] = ma[PartialApply1Of2[Const, B]#Apply, A](c)

  implicit def StateMA[S, A](s: State[S, A]): MA[PartialApply1Of2[State, S]#Apply, A] = ma[PartialApply1Of2[State, S]#Apply, A](s)

  implicit def Tuple2MA[R, A](a: (R, A)): MA[PartialApply1Of2[Tuple2, R]#Apply, A] = ma[PartialApply1Of2[Tuple2, R]#Apply, A](a)

  implicit def Tuple3MA[R, S, A](a: (R, S, A)): MA[PartialApply2Of3[Tuple3, R, S]#Apply, A] = ma[PartialApply2Of3[Tuple3, R, S]#Apply, A](a)

  implicit def Tuple4MA[R, S, T, A](a: (R, S, T, A)): MA[PartialApply3Of4[Tuple4, R, S, T]#Apply, A] = ma[PartialApply3Of4[Tuple4, R, S, T]#Apply, A](a)

  implicit def Tuple5MA[R, S, T, U, A](a: (R, S, T, U, A)): MA[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A] = ma[PartialApply4Of5[Tuple5, R, S, T, U]#Apply, A](a)

  implicit def Tuple6MA[R, S, T, U, V, A](a: (R, S, T, U, V, A)): MA[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A] = ma[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply, A](a)

  implicit def Tuple7MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, A)): MA[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply, A] = ma[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply, A](a)

  implicit def ValidationMA[A, E](v: Validation[E, A]): MA[PartialApply1Of2[Validation, E]#Apply, A] = ma[PartialApply1Of2[Validation, E]#Apply, A](v)

  implicit def ValidationFailureMA[A, E](f: FailProjection[E, A]): MA[PartialApply1Of2[FailProjection, A]#Flip, E] = ma[PartialApply1Of2[FailProjection, A]#Flip, E](f)

  import java.util.Map.Entry

  implicit def MapEntryMA[X, A](e: Entry[X, A]): MA[PartialApply1Of2[Entry, X]#Apply, A] = ma[PartialApply1Of2[Entry, X]#Apply, A](e)

  // Seq[A] implements Function1[Int, A]. Without this, Function1ApplyMA would be used.
  implicit def SeqMA[M[X] <: Seq[X], A](l: M[A]): MA[M, A] = ma[M, A](l)

  // Set[A] implements Function1[Int, B]. Without this, Function1ApplyMA would be used.
  implicit def SetMA[M[X] <: Set[X], A](s: M[A]): MA[M, A] = ma[M, A](s)

  implicit def KleisliMA[M[_], A, B](k: Kleisli[M,A,B]): MA[PartialApplyKA[Kleisli, M, A]#Apply, B] = ma[PartialApplyKA[Kleisli, M, A]#Apply, B](k)

  // move to MABs once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def KleisliMAB[M[_], A, B](k: Kleisli[M, A, B]): MAB[PartialApplyK[Kleisli, M]#Apply, A, B] = mab[PartialApplyK[Kleisli, M]#Apply, A, B](k)

  implicit def CokleisliMAB[M[_], A, B](k: Cokleisli[M, A, B]): MAB[PartialApplyK[Cokleisli, M]#Apply, A, B] = mab[PartialApplyK[Cokleisli, M]#Apply, A, B](k)

  implicit def Const2MAB[M, A, B](k: Const2[M,A,B]): MAB[PartialApply1Of3[Const2,M]#Apply, A, B] =
    mab[PartialApply1Of3[Const2, M]#Apply, A, B](k)
}
