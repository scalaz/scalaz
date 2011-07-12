package scalaz
package sql

import SqlValueT._
import RowValueT._
import PossiblyNullT._

sealed trait RowValueT[F[_], A] {
  val value: F[Either[PossiblyNull[NullMsg], SqlValue[A]]]

  def rowValue(implicit i: F =~~= Identity): RowValue[A] = new RowValue[A] {
    val value =
      Identity.id((i ~~=> RowValueT.this.value).value)
  }

  def toEither: EitherT[PossiblyNull[NullMsg], F, SqlValue[A]] =
    EitherT.eitherT(value)

  def toSqlValue(implicit ftr: Functor[F]): SqlValueT[F, Either[PossiblyNull[NullMsg], A]] =
    eitherSqlValueT[F, Either[PossiblyNull[NullMsg], A]](
      EitherT.eitherT[Err, F, Either[PossiblyNull[NullMsg], A]] (
        ftr.fmap((e: Either[PossiblyNull[NullMsg], SqlValue[A]]) =>
          e match {
            case Left(n) => Right(Left(n))
            case Right(v) => v.fold(
              e => Left(e): Either[Err, Either[PossiblyNull[NullMsg], A]]
            , a => Right(Right(a))
            )
          })(value)))

  def toPossiblyNull(implicit ftr: Functor[F]): PossiblyNullT[({type λ[α] = EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A] =
    PossiblyNullT.fromOptionT[({type λ[α] = EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A](
      OptionT[({type λ[α] = EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A](
        EitherT[String,({type λ[α] = SqlValueT[F, α]})#λ,Option[A]](
          eitherSqlValueT[F, Either[String, Option[A]]](
            EitherT(
              ftr.fmap((e: Either[PossiblyNull[NullMsg], SqlValue[A]]) =>
                e match {
                  case Left(n) => n.fold(
                    m => Right(Left(m))
                  , Right(Right(None))
                  )
                  case Right(v) => v.fold[Either[Err,Either[NullMsg,Option[A]]]](
                    e => Left(e)
                  , a => Right(Right(Some(a)))
                  )
                })(value))))))
}

object RowValueT extends RowValueTs {
  def apply[F[_], A](a: F[A])(implicit ftr: Functor[F]): RowValueT[F, A] =
    rowValueT(a)
}

trait RowValueTs {
  type RowValue[A] =
  RowValueT[Identity, A]

  type NullMsg =
  String

  def rowValueT[F[_], A](a: F[A])(implicit ftr: Functor[F]): RowValueT[F, A] = new RowValueT[F, A] {
    val value =
      ftr.fmap((aa: A) =>
        Right(SqlValueT.sqlValue(aa)): Either[PossiblyNull[NullMsg], SqlValue[A]])(a)
  }

  def rowValue[A]: A => RowValue[A] =
    a => rowValueT(Identity.id(a))

  def rowErrorT[F[_], A](a: F[Err])(implicit ftr: Functor[F]): RowValueT[F, A] = new RowValueT[F, A] {
    val value =
      ftr.fmap((e: Err) =>
        Right(sqlError(e)): Either[PossiblyNull[NullMsg], SqlValue[A]])(a)
  }

  def rowError[A]: Err => RowValue[A] =
    e => rowErrorT(Identity.id(e))

  def rowNullT[F[_], A](implicit p: Pointed[F]): RowValueT[F, A] = new RowValueT[F, A] {
    val value =
      p.point(Left(isNull): Either[PossiblyNull[NullMsg], SqlValue[A]])
  }

  def rowNull[A]: RowValue[A] =
    rowNullT

  def rowNullMsgT[F[_], A](m: NullMsg)(implicit p: Pointed[F]): RowValueT[F, A] = new RowValueT[F, A] {
    val value =
      p.point(Left(notNull(m)): Either[PossiblyNull[NullMsg], SqlValue[A]])
  }

  def rowNullMsg[A]: NullMsg => RowValue[A] =
    rowNullMsgT(_)

}