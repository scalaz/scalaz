// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

trait IOInstances {
  implicit val monad: Monad[IO] = new Monad[IO] {
//    override final def map[A, B](ma: IO[A])(f: A => B): IO[B] =
//      ma.map(f)
//
//    override final def ap[A, B](ma: IO[A])(mf: IO[A => B]): IO[B] =
//      ma.flatMap(a => mf.map(f => f(a)))




//////    override final def pure[A](a: A): IO[A] = IO.now(a)
////
////    override final def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] =
//      ma.flatMap(f)
    def point[A](a: => A): IO[A] = IO.point(a)

    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  }
}
