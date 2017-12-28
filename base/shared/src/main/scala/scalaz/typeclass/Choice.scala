package scalaz
package typeclass

trait Choice[P[_, _]] {
  def profunctor: Profunctor[P]

  def leftchoice[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C]

  def rightchoice[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B]
}
