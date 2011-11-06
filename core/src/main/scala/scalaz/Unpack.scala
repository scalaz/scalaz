package scalaz


/** Represents a type `MA` that has been unpacked into `M[A]`. */
trait UnpackM[MA] {
  type M[_]
  type A
  def apply(ma: MA): M[A]
}

// Destructuring implicits
object UnpackM {
  implicit def unpackM[M0[_], A0] = new UnpackM[M0[A0]] {
    type M[X] = M0[X]
    type A = A0
    def apply(ma: M0[A0]) = ma
  }

  // Ambiguity ruins the party. Easy solution is to selectively import non-overlapping
  // instances of unpackXxx where needed.
  //
  // Better would be use the availability of an typeclass drive the selection. This probably
  // needs more more support from the compiler.
  //
  /*implicit def unpackMBin1[M0[_, _], A0, B0] = new UnpackM[M0[A0, B0]] {
    type M[X] = M0[X, B0]
    type A = A0
    def apply(ma: M0[A0, B0]) = ma
  }*/

  implicit def unpackMBin2[M0[_, _], A0, B0] = new UnpackM[M0[A0, B0]] {
    type M[X] = M0[A0, X]
    type A = B0
    def apply(ma: M0[A0, B0]) = ma
  }
}
