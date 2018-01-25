// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.typeclass.{Bind, Monad}

trait IOInstances {
  implicit val monad: Monad[IO] = new Monad.Template[IO] with Bind.DeriveFlatten[IO] {
    override final def map[A, B](ma: IO[A])(f: A => B): IO[B] =
      ma.map(f)

    override final def ap[A, B](ma: IO[A])(mf: IO[A => B]): IO[B] =
      ma.flatMap(a => mf.map(f => f(a)))

    override final def pure[A](a: A): IO[A] = IO.now(a)

    override final def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
      ma.flatMap(f)
  }
}
