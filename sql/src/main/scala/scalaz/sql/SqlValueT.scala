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

  implicit def SqlValueTFoldr[F[_]: Foldr]: Foldr[({type λ[α] = SqlValueT[F, α]})#λ] = new Foldr[({type λ[α] = SqlValueT[F, α]})#λ] {
    def foldr[A, B] = k => b => s =>
      implicitly[Foldr[({type λ[α] = EitherT[SqlException, F, α]})#λ]].foldr(k)(b)(s.value)
  }

  implicit def SqlValueTFoldl[F[_]: Foldl]: Foldl[({type λ[α] = SqlValueT[F, α]})#λ] = new Foldl[({type λ[α] = SqlValueT[F, α]})#λ] {
    def foldl[A, B] = k => b => s =>
      implicitly[Foldl[({type λ[α] = EitherT[SqlException, F, α]})#λ]].foldl(k)(b)(s.value)
  }

  implicit val SqlValueTMonadTrans: MonadTrans[SqlValueT] = new MonadTrans[SqlValueT] {
    def lift[G[_] : Monad, A](a: G[A]): SqlValueT[G, A] =
      new SqlValueT[G, A] {
        val value = implicitly[MonadTrans[({type λ[α[_], β] = EitherT[SqlException, α, β]})#λ]].lift(a)
      }
  }

}
