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
    with    LastOptions
    with    LazyTuples
    with    Lists
    with    Longs
    with    MAs
    with    MetricSpaces
    with    Memos
    with    Multiplications
    with    Names
    with    NonEmptyLists
    with    Options
    with    Orders
    with    Promises
    with    Reducers
    with    Semigroups 
    with    FingerTree.IndSeqs
    with    FingerTree.Ropes
    with    Shorts
    with    Shows
    with    States
    with    Streams
    with    Strings
    with    Trees
    with    Tuples
    with    TreeLocs
    with    Validations
    with    Zeros
    with    Zippers
    with    ZipStreams {
  def ⊥ = system.error("undefined")

  def undefined = ⊥

  type ⊤ = Any

  type ⊥ = Nothing

  type ℤ = scala.math.BigInt

  lazy val π = java.lang.Math.PI

  lazy val π2 = π * 2

  type ArraySeq[A] = collection.mutable.ArraySeq[A]

  val ArraySeq = collection.mutable.ArraySeq

  def ×[A, B] = (a: A) => (b: B) => (a, b)

  def pure[F[_]:Pure] = new (Id ~> F) {
    def apply[A](a: A) = implicitly[Pure[F]].pure(a)
  }

  import scala.collection.generic.CanBuildFrom
  
  // todo move these to MAs, once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def EitherLeftMA[X, A](a: Either.LeftProjection[A, X]) = ma[({type λ[α]=Either.LeftProjection[α, X]})#λ, A](a)

  implicit def EitherRightMA[X, A](a: Either.RightProjection[X, A]): MA[({type λ[α]=Either.RightProjection[X, α]})#λ, A] = ma[({type λ[α]=Either.RightProjection[X, α]})#λ, A](a)

  implicit def Function1FlipMACofunctor[A, R](f: R => A): MACofunctor[({type λ[α]=(α) => A})#λ, R] = maCofunctor[({type λ[α]=(α) => A})#λ, R](f)

  implicit def Function1ApplyMA[A, R](f: A => R): MA[({type λ[α]=(A) => α})#λ, R] = ma[({type λ[α]=(A) => α})#λ, R](f)

  implicit def Function2MA[R, S, A](a: (R, S) => A): MA[({type λ[α]=(R, S) => α})#λ, A] = ma[({type λ[α]=(R, S) => α})#λ, A](a)

  implicit def Function3MA[R, S, T, A](a: (R, S, T) => A): MA[({type λ[α]=(R, S, T) => α})#λ, A] = ma[({type λ[α]=(R, S, T) => α})#λ, A](a)

  implicit def Function4MA[R, S, T, U, A](a: (R, S, T, U) => A): MA[({type λ[α]=(R, S, T, U) => α})#λ, A] = ma[({type λ[α]=(R, S, T, U) => α})#λ, A](a)

  implicit def Function5MA[R, S, T, U, V, A](a: (R, S, T, U, V) => A): MA[({type λ[α]=(R, S, T, U, V) => α})#λ, A] = ma[({type λ[α]=(R, S, T, U, V) => α})#λ, A](a)

  implicit def Function6MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => A): MA[({type λ[α]=(R, S, T, U, V, W) => α})#λ, A] = ma[({type λ[α]=(R, S, T, U, V, W) => α})#λ, A](a)

  implicit def ConstMA[B, A](c: Const[B, A]): MA[({type λ[α]=Const[B, α]})#λ, A] = ma[({type λ[α]=Const[B, α]})#λ, A](c)

  implicit def StateMA[S, A](s: State[S, A]): MA[({type λ[α]=State[S, α]})#λ, A] = ma[({type λ[α]=State[S, α]})#λ, A](s)

  implicit def Tuple2MA[R, A](a: (R, A)): MA[({type λ[α]=(R, α)})#λ, A] = ma[({type λ[α]=(R, α)})#λ, A](a)

  implicit def Tuple3MA[R, S, A](a: (R, S, A)): MA[({type λ[α]=(R, S, α)})#λ, A] = ma[({type λ[α]=(R, S, α)})#λ, A](a)

  implicit def Tuple4MA[R, S, T, A](a: (R, S, T, A)): MA[({type λ[α]=(R, S, T, α)})#λ, A] = ma[({type λ[α]=(R, S, T, α)})#λ, A](a)

  implicit def Tuple5MA[R, S, T, U, A](a: (R, S, T, U, A)): MA[({type λ[α]=(R, S, T, U, α)})#λ, A] = ma[({type λ[α]=(R, S, T, U, α)})#λ, A](a)

  implicit def Tuple6MA[R, S, T, U, V, A](a: (R, S, T, U, V, A)): MA[({type λ[α]=(R, S, T, U, V, α)})#λ, A] = ma[({type λ[α]=(R, S, T, U, V, α)})#λ, A](a)

  implicit def Tuple7MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, A)): MA[({type λ[α]=(R, S, T, U, V, W, α)})#λ, A] = ma[({type λ[α]=(R, S, T, U, V, W, α)})#λ, A](a)

  implicit def ValidationMA[A, E](v: Validation[E, A]): MA[({type λ[α]=Validation[E, α]})#λ, A] = ma[({type λ[α]=Validation[E, α]})#λ, A](v)

  implicit def ValidationFailureMA[A, E](f: FailProjection[E, A]): MA[({type λ[α]=FailProjection[α, A]})#λ, E] = ma[({type λ[α]=FailProjection[α, A]})#λ, E](f)
  
  implicit def IterVMA[A, E](v: IterV[E, A]): MA[({type λ[α]=IterV[E, α]})#λ, A] = ma[({type λ[α]=IterV[E, α]})#λ, A](v)

  import java.util.Map.Entry

  implicit def MapEntryMA[X, A](e: Entry[X, A]): MA[({type λ[α]=Entry[X, α]})#λ, A] = ma[({type λ[α]=Entry[X, α]})#λ, A](e)

  // Seq[A] implements Function1[Int, A]. Without this, Function1ApplyMA would be used.
  implicit def SeqMA[M[X] <: Seq[X], A](l: M[A]): MA[M, A] = ma[M, A](l)

  // Set[A] implements Function1[Int, B]. Without this, Function1ApplyMA would be used.
  implicit def SetMA[M[X] <: Set[X], A](s: M[A]): MA[M, A] = ma[M, A](s)

  implicit def KleisliMA[M[_], A, B](k: Kleisli[M,A,B]): MA[({type λ[α]=Kleisli[M, A, α]})#λ, B] = ma[({type λ[α]=Kleisli[M, A, α]})#λ, B](k)

  implicit def FingerTreeMA[V, A](t: FingerTree[V, A]): MA[({type λ[α]=FingerTree[V, α]})#λ, A] = ma[({type λ[α]=FingerTree[V, α]})#λ, A](t)

  implicit def FingerMA[V, A](t: Finger[V, A]): MA[({type λ[α]=Finger[V, α]})#λ, A] = ma[({type λ[α]=Finger[V, α]})#λ, A](t)

  implicit def NodeMA[V, A](t: Node[V, A]): MA[({type λ[α]=Node[V, α]})#λ, A] = ma[({type λ[α]=Node[V, α]})#λ, A](t)

  implicit def MemoMA[V, A](m: Memo[A, V]): MA[({type λ[α]=Memo[α, V]})#λ, A] = ma[({type λ[α]=Memo[α, V]})#λ, A](m)

  // move to MABs once https://lampsvn.epfl.ch/trac/scala/ticket/2741 is solved.
  implicit def KleisliMAB[M[_], A, B](k: Kleisli[M, A, B]): MAB[({type λ[α, β]=Kleisli[M, α, β]})#λ, A, B] = mab[({type λ[α, β]=Kleisli[M, α, β]})#λ, A, B](k)

  implicit def CokleisliMAB[M[_], A, B](k: Cokleisli[M, A, B]): MAB[({type λ[α, β]=Cokleisli[M, α, β]})#λ, A, B] = mab[({type λ[α, β]=Cokleisli[M, α, β]})#λ, A, B](k)

  implicit def Const2MAB[M, A, B](k: Const2[M,A,B]): MAB[({type λ[α, β]=Const2[M, α, β]})#λ, A, B] =
    mab[({type λ[α, β]=Const2[M, α, β]})#λ, A, B](k)
}
