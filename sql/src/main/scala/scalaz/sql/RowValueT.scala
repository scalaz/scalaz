package scalaz
package sql

import SqlValueT._
import RowValueT._

sealed trait RowValueT[F[_], A] {
  val value: EitherT[Option[NullMsg], ({type λ[α] = SqlValueT[F, α]})#λ, A]

  def switch(implicit ftr: Functor[F]): OptionT[({type λ[α] = EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A] =
    OptionT[({type λ[α] = EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A](
      EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, Option[A]](
        SqlValueT(
          EitherT(
            ftr.fmap((e: Either[Err, Either[Option[NullMsg], A]]) =>
              (e.right map {
                case Left(Some(s)) => Left(s)
                case Left(None) => Right(None)
                case Right(a) => Right(Some(a))
              }): Either[Err, Either[NullMsg, Option[A]]]
            )(value.runT.value.runT)))))
}

object RowValueT extends RowValueTs {
  def apply[F[_], A](x: EitherT[Option[NullMsg], ({type λ[α] = SqlValueT[F, α]})#λ, A]): RowValueT[F, A] = new RowValueT[F, A] {
    val value = x
  }
}

trait RowValueTs {
  type RowValue[A] =
  RowValueT[Identity, A]

  type NullMsg =
  String
}