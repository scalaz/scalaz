package scalaz
package data


trait KleisliInstances {

  implicit def kleisliCompose[M[_], A, B](implicit M: Monad[M]): Compose[Kleisli[M, ?, ?]] = new Compose[Kleisli[M, ?, ?]] {
    override def compose[A, B, C](f: Kleisli[M, B, C], g: Kleisli[M, A, B]): Kleisli[M, A, C] =
      Kleisli.wrapKleisli(a => M.bind.flatMap(Kleisli.runKleisli(g)(a))(Kleisli.runKleisli(f)))
  }
}
