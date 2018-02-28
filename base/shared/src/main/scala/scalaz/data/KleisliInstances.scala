package scalaz
package data

import scalaz.typeclass.ComposeClass

trait KleisliInstances {

  implicit def kleisliCompose[M[_]](implicit M: Monad[M]): Compose[Kleisli[M, ?, ?]] =
    instanceOf(new ComposeClass[Kleisli[M, ?, ?]] {
      override def compose[A, B, C](f: Kleisli[M, B, C], g: Kleisli[M, A, B]): Kleisli[M, A, C] =
        Kleisli.wrapKleisli(a => M.flatMap(Kleisli.runKleisli(g)(a))(Kleisli.runKleisli(f)))
    })
}
