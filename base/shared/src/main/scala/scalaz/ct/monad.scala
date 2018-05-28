package scalaz
package ct

import scala.{ Function0, Function1, List, Option }

trait MonadClass[M[_]] extends ApplicativeClass[M] with BindClass[M]

object MonadClass {

  trait DeriveMap[M[_]] extends MonadClass[M] with BindClass.Alt[BindClass.DeriveFlatten[M]] {
    final override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  trait Alt[D <: Alt[D]]
}

trait MonadInstances {
  implicit val optionMonad: Monad[Option] = instanceOf(new MonadClass[Option] with BindClass.DeriveFlatten[Option] {
    override def ap[A, B](oa: Option[A])(f: Option[A => B]): Option[B]      = oa.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)
    override def map[A, B](oa: Option[A])(f: A => B): Option[B]             = oa.map(f)
    override def pure[A](a: A): Option[A]                                   = Option(a)
  })

  implicit val listMonad: Monad[List] = instanceOf(new MonadClass[List] with BindClass.DeriveFlatten[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B]      = xs.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B]           = xs.map(f)
    override def pure[A](a: A): List[A]                               = List(a)
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
}
