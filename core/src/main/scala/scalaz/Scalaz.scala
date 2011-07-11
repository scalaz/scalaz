package scalaz

object Scalaz extends
Alphas with
Applics with
Applicatives with
Binds with
BKTrees with
Categorys with
CoKleislis with
Comps with
Consts with
CoStateTs with
Dequeues with
Digits with
EitherTs with
Endos with
EphemeralStreams with
Equals with
FailProjections with
Foldls with
Functors with
Heaps with
Identitys with
ImmutableArrays with
Joins with
Kleislis with
LazyEithers with
LazyEitherTs with
LazyOptions with
LazyOptionTs with
LazyTuples with
Lenss with
Monads with
Monoids with
NonEmptyLists with
OptionTs with
Orders with
Pointeds with
PointedFunctors with
ReaderWriterStateTs with
Recursion with
Semigroups with
Shows with
StateTs with
StepListTs with
StepStreamTs with
Trees with
TreeLocs with
Validations with
WriterTs with
Zeros with
Zippers with
concurrent.Concurrents with
effect.Effects with
iteratee.Iteratees with
newtypes.Newtypess with
wrap.Wraps with
Scalazs with
** with
**->** with
**->**->** with
~>> with
^^*^^ with
~** with
!** with
==~~==

object Scalazing extends Scalazs

trait Scalazs extends {

  import ~>._

  type ⊤ = Any

  type ⊥ = Nothing

  type ℤ = scala.math.BigInt

  val π = java.lang.Math.PI

  val τ = π * 2

  type ArraySeq[A] = collection.mutable.ArraySeq[A]

  val ArraySeq = collection.mutable.ArraySeq

  def ×[A, B] = (a: A) => (b: B) => (a, b)

  def point[F[_] : Pointed]: (I ~> F) = new (I ~> F) {
    def apply[A](a: A) = implicitly[Pointed[F]].point(a)
  }

  def ∅[Z](implicit z: Zero[Z]): Z =
    z.zero

  def mzero[Z](implicit z: Zero[Z]): Z =
    z.zero
}
