package scalaz
package sql

import Kleisli._

sealed trait ConnectT[F[_], A] {
  val connect: ReaderT[Connection, ({type λ[α] = SqlValueT[F, α]})#λ, A]
}
