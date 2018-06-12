package scalaz

package object ct {

  val Compose: ComposeModule = ComposeImpl
  type Compose[F[_], G[_], X] = Compose.Compose[F, G, X]

  val Endo: EndoModule = EndoImpl
  type Endo[=>:[_, _], A] = Endo.Endo[=>:, A]

  val Kleisli: KleisliModule = KleisliImpl
  type Kleisli[F[_], A, B] = Kleisli.Kleisli[F, A, B]
}
