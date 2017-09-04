package scalaz

package object data {
  val Forall: ForallModule with ForallSyntax = ForallImpl
  val ∀ : Forall.type = Forall

  type Forall[F[_]] = Forall.Forall[F]
  type ∀[F[_]] = Forall[F]

  val Forall2: Forall2Module with Forall2Syntax = Forall2Impl
  val ∀∀ : Forall2.type = Forall2

  type Forall2[F[_, _]] = Forall2.Forall2[F]
  type ∀∀[F[_, _]] = Forall2[F]
}
