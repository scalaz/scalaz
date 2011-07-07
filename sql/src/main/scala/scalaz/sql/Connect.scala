package scalaz
package sql

sealed trait Connect[F[_], A] {
  val connect: Connection => SqlValue[F, A]
}
