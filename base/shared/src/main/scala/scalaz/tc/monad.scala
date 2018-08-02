package scalaz
package tc

import scala._
import scala.Predef.$conforms

import zio.IO

trait MonadClass[M[_]] extends ApplicativeClass[M] with BindClass[M]

object MonadClass {

  trait DeriveMap[M[_]] extends MonadClass[M] with BindClass.Alt[BindClass.DeriveFlatten[M]] {
    final override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  trait Alt[D <: Alt[D]]

  implicit def eitherMonad[L]: Monad[Either[L, ?]] =
    instanceOf(new MonadClass[Either[L, ?]] {
      def pure[A](a: A): Either[L, A]                                         = Right(a)
      def ap[A, B](fa: Either[L, A])(f: Either[L, A => B]): Either[L, B]      = fa.flatMap(a => f.map(fab => fab(a)))
      def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] = ma.flatMap(f)
      def flatten[A](ma: Either[L, Either[L, A]]): Either[L, A]               = ma.flatMap(x => x)
      def map[A, B](ma: Either[L, A])(f: A => B): Either[L, B]                = ma.map(f)
    })

  implicit val functionMonad: Monad[Function0] = instanceOf(
    new MonadClass[Function0] with BindClass.DeriveFlatten[Function0] {
      override def ap[A, B](fab: Function0[A])(f: Function0[A => B]): Function0[B]      = () => f()(fab())
      override def map[A, B](fab: Function0[A])(f: A => B): Function0[B]                = () => f(fab())
      override def flatMap[A, B](fab: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(fab())()
      override def pure[A](a: A): Function0[A]                                          = () => a
    }
  )

  implicit def function1Monad[C]: Monad[Function1[C, ?]] =
    instanceOf(new MonadClass[Function1[C, ?]] with BindClass.DeriveFlatten[Function1[C, ?]] {
      override def ap[A, B](fab: Function1[C, A])(f: Function1[C, A => B]): Function1[C, B] = (c: C) => f(c)(fab(c))
      override def map[A, B](fab: Function1[C, A])(f: A => B): Function1[C, B]              = fab andThen f
      override def flatMap[A, B](fab: Function1[C, A])(f: A => Function1[C, B]): Function1[C, B] =
        (c: C) => f(fab(c))(c)
      override def pure[A](a: A): Function1[C, A] = (c: C) => a
    })

  implicit def ioMonad[E]: Monad[IO[E, ?]] =
    instanceOf(new MonadClass[IO[E, ?]] with BindClass.DeriveFlatten[IO[E, ?]] {
      override final def ap[A, B](ma: IO[E, A])(mf: IO[E, A => B]): IO[E, B] =
        ma.flatMap(a => mf.map(f => f(a)))
      override final def flatMap[A, B](ma: IO[E, A])(f: A => IO[E, B]): IO[E, B] =
        ma.flatMap(f)
      override final def map[A, B](ma: IO[E, A])(f: A => B): IO[E, B] =
        ma.map(f)
      override final def pure[A](a: A): IO[E, A] = IO.now(a)
    })

  implicit val listMonad: Monad[List] = instanceOf(new MonadClass[List] with BindClass.DeriveFlatten[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B]      = xs.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B]           = xs.map(f)
    override def pure[A](a: A): List[A]                               = List(a)
  })

  implicit val optionMonad: Monad[Option] =
    instanceOf(new MonadClass[Option] {
      def pure[A](a: A): Option[A]                                   = Some(a)
      def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B]      = fa.flatMap(a => f.map(fab => fab(a)))
      def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
      def flatten[A](ma: Option[Option[A]]): Option[A]               = ma.flatten
      def map[A, B](ma: Option[A])(f: A => B): Option[B]             = ma.map(f)
    })

  implicit val vectorMonad: Monad[Vector] = instanceOf(new MonadClass[Vector] {
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B]             = fa.map(f)
    override def ap[A, B](fa: Vector[A])(f: Vector[A => B]): Vector[B]      = fa.zip(f).map { case (e, f) => f(e) }
    override def pure[A](a: A): Vector[A]                                   = Vector(a)
    override def flatMap[A, B](oa: Vector[A])(f: A => Vector[B]): Vector[B] = oa.flatMap(f)
    override def flatten[A](ma: Vector[Vector[A]]): Vector[A]               = ma.flatten
  })

}
