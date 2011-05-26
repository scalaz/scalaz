package scalaz

object Scalaz extends
Applics with
Applicatives with
Binds with
Categorys with
Comps with
Consts with
Equals with
Foldables with
Foldls with
Functors with
Joins with
Monads with
Monoids with
~>> with
^^*^^ with
Orders with
Pointeds with
PointedFunctors with
Semigroups with
Shows with
Zeros with
concurrent.Concurrents with
data.Datas with
effect.Effects with
iteratee.Iteratees with
newtypes.Newtypess with
wrap.Wraps with
Scalazs

object Scalazing extends Scalazs

trait Scalazs extends {

  import ~>._

  type ⊤ = Any

  type ⊥ = Nothing

  type ℤ = scala.math.BigInt

  val π = java.lang.Math.PI

  val π2 = π * 2

  type ArraySeq[A] = collection.mutable.ArraySeq[A]

  val ArraySeq = collection.mutable.ArraySeq

  def ×[A, B] = (a: A) => (b: B) => (a, b)

  def point[F[_] : Pointed]: (I ~> F) = new (I ~> F) {
    def apply[A](a: A) = implicitly[Pointed[F]].point(a)
  }

  def ∅[Z](implicit z: Zero[Z]): Z =
    z.zero
}
