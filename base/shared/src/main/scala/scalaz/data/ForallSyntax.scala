package scalaz
package data

trait ForallSyntax {
  implicit class Ops[F[_]](val a: ∀[F]) {
    def of[A]: F[A] = Forall.specialize(a)
    def apply[A]: F[A] = of[A]
  }
}

object ForallSyntax extends ForallSyntax