package scalaz
package sql

import Kleisli._

sealed trait Connect[F[_], A] {
  val connect: ReaderT[Connection, ({type λ[α] = SqlValue[F, α]})#λ, A]
}
