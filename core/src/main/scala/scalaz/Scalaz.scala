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
  def ⊥ = error("undefined")

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
}
