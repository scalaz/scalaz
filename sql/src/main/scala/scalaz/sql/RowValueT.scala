package scalaz
package sql

import SqlValueT._
import RowValueT._
import PossiblyNullT._

sealed trait RowValueT[F[_], A] {
  val value: F[Either[PossiblyNull[NullMsg], SqlValue[A]]]

  def *->* : (({type λ[α] = RowValueT[F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = RowValueT[F, α]})#λ, A](this)

  import =~~=._

  def fold[X](e: Err => X, a: A => X, m: NullMsg => X, n: => X)(implicit i: F =~~= Identity): X =
    foldT(e, a, m, n)(new Functor[F] {
        def fmap[A, B](f: A => B) =
          k => <=~~[F, B](f(~~=>(k)(i)))
      })

  def foldT[X](e: Err => X, a: A => X, m: NullMsg => X, n: => X)(implicit ftr: Functor[F]): F[X] =
    ftr.fmap((_: Either[PossiblyNull[NullMsg], SqlValue[A]]).fold(
      _.fold(
        m
      , n
      )
    , _.fold(
        e
      , a
      )
    ))(value)

  def foldOrNullMsg[X](defaultNullMsg: => NullMsg)(e: Err => X, a: A => X, nul: NullMsg => X)(implicit i: F =~~= Identity): X =
    foldOrNullMsgT(defaultNullMsg)(e, a, nul)(new Functor[F] {
        def fmap[A, B](f: A => B) =
          k => <=~~[F, B](f(~~=>(k)(i)))
      })

  def foldOrNullMsgT[X](defaultNullMsg: => NullMsg)(e: Err => X, a: A => X, nul: NullMsg => X)(implicit ftr: Functor[F]): F[X] =
    foldT(e, a, nul, nul(defaultNullMsg))

  final def loop[X](e: Err => X, v: A => Either[X, RowValue[A]], m: NullMsg => X, n: => X)(implicit i: F =~~= Identity): X =
    loopT(e, v, m, n)(new Functor[F] {
        def fmap[A, B](f: A => B) =
          k => <=~~[F, B](f(~~=>(k)(i)))
      })

  final def loopT[X](e: Err => X, v: A => Either[X, RowValue[A]], m: NullMsg => X, n: => X)(implicit ftr: Functor[F]): F[X] =
    ftr.fmap((x: Either[PossiblyNull[NullMsg], SqlValue[A]]) => {
      @annotation.tailrec
      def spin(y: Either[PossiblyNull[NullMsg], SqlValue[A]]): X =
        y match {
          case Left(k) => k.fold(m, n)
          case Right(w) => w.toEither match {
            case Left(d) => e(d)
            case Right(a) => v(a) match {
              case Left(x) => x
              case Right(r) => spin(r.toEither)
            }
          }
        }
      spin(x)
    })(value)

  def toEither(implicit i: F =~~= Identity): Either[PossiblyNull[NullMsg], SqlValue[A]] =
    value

  def toEitherT: EitherT[PossiblyNull[NullMsg], F, SqlValue[A]] =
    EitherT.eitherT(value)

  def toSqlValueT(implicit ftr: Functor[F]): SqlValueT[F, Either[PossiblyNull[NullMsg], A]] =
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

  def toPossiblyNullT(implicit ftr: Functor[F]): PossiblyNullT[({type λ[α] = EitherT[NullMsg, ({type λ[α] = SqlValueT[F, α]})#λ, α]})#λ, A] =
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

  def fromSqlValue[F[_], A](v: SqlValueT[F, A])(implicit ftr: Functor[F]): RowValueT[F, A] = new RowValueT[F, A] {
    val value =
      ftr.fmap((e: Either[Err, A]) => Right(e.fold(sqlError, sqlValue)): Either[PossiblyNull[NullMsg], SqlValue[A]])(v.value.runT)
  }
}