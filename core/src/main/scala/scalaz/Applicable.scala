package scalaz

trait Applicable { type Apply[A] }

trait Curry[T[_, _]] extends Applicable {
  trait C[Z] extends Applicable {
    type Apply[Y] = T[Z, Y]
  }
  type Apply[X] = C[X]
}

trait Flip[T[_, _]] extends Applicable { type Apply[X] = Curry[T]#Apply[X] }
