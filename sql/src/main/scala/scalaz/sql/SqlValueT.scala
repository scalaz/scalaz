package scalaz
package sql

import SqlValueT._

sealed trait SqlValueT[F[_], A] {
  val value: EitherT[SqlException, F, A]
}

object SqlValueT extends SqlValueTs

trait SqlValueTs {
  type SqlException =
  java.sql.SQLException

  type SqlValue[A] =
  SqlValueT[Identity, A]

  def sqlValueT[F[_], A](a: F[A])(implicit ftr: Functor[F]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = EitherT.eitherT(ftr.fmap((a: A) => Right(a): Either[SqlException, A])(a))
  }

  def sqlErrorT[F[_], A](a: F[SqlException])(implicit ftr: Functor[F]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = EitherT.eitherT(ftr.fmap((e: SqlException) => Left(e): Either[SqlException, A])(a))
  }

  def sqlValue[A]: A => SqlValue[A] =
    a => sqlValueT(Identity.id(a))

  def sqlError[A]: SqlException => SqlValue[A] =
    e => sqlErrorT(Identity.id(e))

  implicit val SqlValueTMonadTrans: MonadTrans[SqlValueT] = new MonadTrans[SqlValueT] {
    def lift[G[_] : Monad, A](a: G[A]): SqlValueT[G, A] =
      new SqlValueT[G, A] {
        val value = implicitly[MonadTrans[({type λ[α[_], β] = EitherT[SqlException, α, β]})#λ]].lift(a)
      }
  }

}
