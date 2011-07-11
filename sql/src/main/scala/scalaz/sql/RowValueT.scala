package scalaz
package sql

import SqlValueT._

sealed trait RowValueT[F[_], A] {
  val value: EitherT[Option[String], ({type λ[α] = SqlValueT[F, α]})#λ, A]

  def switch(implicit ftr: Functor[F]): OptionT[({type λ[α] = EitherT[String, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A] =
    OptionT[({type λ[α] = EitherT[String, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A](
      EitherT[String, ({type λ[α] = SqlValueT[F, α]})#λ, Option[A]](
        SqlValueT(
          EitherT(
            ftr.fmap((e: Either[Err, Either[Option[String], A]]) =>
              (e.right map {
                case Left(Some(s)) => Left(s)
                case Left(None) => Right(None)
                case Right(a) => Right(Some(a))
              }): Either[Err, Either[String, Option[A]]]
            )(value.runT.value.runT)))))
}

object RowValue extends RowValues {
  def apply[F[_], A](x: EitherT[Option[String], ({type λ[α] = SqlValueT[F, α]})#λ, A]): RowValueT[F, A] = new RowValueT[F, A] {
    val value = x
  }
}

trait RowValues {
  type RowValue[A] =
  RowValueT[Identity, A]
}