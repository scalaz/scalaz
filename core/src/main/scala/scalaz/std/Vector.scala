package scalaz
package std

trait IndexedSeqSubVector extends IndexedSeqSub {
  type IxSq[+A] = Vector[A]
  protected final def buildIxSq[A, B] = implicitly
  protected final def covariant = vector.vectorInstance
  protected final def empty[A] = Vector()
}

sealed trait VectorInstances0 {
  implicit def vectorEqual[A](implicit A0: Equal[A]): Equal[Vector[A]] = new IndexedSeqEqual[A, Vector[A]] {
    implicit def A = A0
  }
}

trait VectorInstances extends VectorInstances0 {
  object generic extends IndexedSeqSubVector with IndexedSeqSubInstances

  implicit val vectorInstance = generic.ixSqInstance

  // Vector concat is O(n^2) in Scala 2.10 - it's actually faster to do repeated appends
  // https://issues.scala-lang.org/browse/SI-7725
  //
  // It was reduced to O(n) in Scala 2.11 - ideally it would be O(log n)
  // https://issues.scala-lang.org/browse/SI-4442
  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    def zero = Vector.empty
    def append(a: Vector[A], b: => Vector[A]) = b.foldLeft(a)(_ :+ _)
  }

  implicit def vectorShow[A: Show]: Show[Vector[A]] = generic.ixSqShow

  implicit def vectorOrder[A](implicit A0: Order[A]): Order[Vector[A]] = generic.ixSqOrder
}

object vector extends IndexedSeqSubVector with VectorInstances with IndexedSeqSubFunctions {
  object vectorSyntax extends scalaz.syntax.std.ToVectorOps
}
