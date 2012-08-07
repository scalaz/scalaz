package scalaz
package std

trait IndexedSeqSubVector extends IndexedSeqSub {
  type IxSq[+A] = Vector[A]
  protected final def buildIxSq[A, B] = implicitly
  protected final def monad = vector.vectorInstance
  protected final def empty[A] = Vector()
}

trait VectorInstances0 {
  implicit def vectorEqual[A](implicit A0: Equal[A]) = new IndexedSeqEqual[A, Vector[A]] {
    implicit def A = A0
  }
}

trait VectorInstances extends VectorInstances0 {
  object generic extends IndexedSeqSubVector with IndexedSeqSubInstances

  implicit val vectorInstance = generic.ixSqInstance

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = generic.ixSqMonoid

  implicit def vectorShow[A: Show]: Show[Vector[A]] = generic.ixSqShow

  implicit def vectorOrder[A](implicit A0: Order[A]): Order[Vector[A]] = generic.ixSqOrder
}

object vector extends IndexedSeqSubVector with VectorInstances with IndexedSeqSubFunctions {
  object vectorSyntax extends scalaz.syntax.std.ToVectorOps
}
