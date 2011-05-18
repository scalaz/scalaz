package scalaz

trait ~>[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

object ~> extends ~>>

trait ~>> {
  type I[A] = A

  trait K[A] {
    type Apply[B] = A
  }

  type Thunk[A] = () => A

  /**A function type encoded as a natural transformation */
  type ->[A, B] = K[A]#Apply ~> K[B]#Apply

  /**A universally quantified identity function */
  def id = new (I ~> I) {
    def apply[A](a: A) = a
  }

  implicit def NaturalTransformationFunction[F[_], G[_], A](f: F ~> G): F[A] => G[A] =
    x => f(x)
}
