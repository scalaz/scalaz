package scalaz
package data

trait Forall2Syntax {
  implicit final class Ops[F[_, _]](val a: ∀∀[F]) {
    def of[A, B]: F[A, B] = Forall2.specialize(a)
    def apply[A, B]: F[A, B] = of[A, B]
  }
}

object Forall2Syntax extends Forall2Syntax