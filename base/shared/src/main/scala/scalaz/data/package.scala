package scalaz

package object data {
  val forall: ForallModule with ForallSyntax = ForallImpl
  val ∀ : forall.type = forall

  type Forall[F[_]] = forall.Forall[F]
  type ∀[F[_]] = Forall[F]
}
