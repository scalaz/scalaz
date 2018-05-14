// Copyright (C) 2018 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.{ instanceOf, Applicative, Monoid }
import scalaz.typeclass.{ ApplicativeClass, MonoidClass }

trait FiberInstances {
  implicit def fiberMonoid[E, A](implicit A: Monoid[A]): Monoid[Fiber[E, A]] =
    instanceOf(new MonoidClass[Fiber[E, A]] {
      final def append(a1: Fiber[E, A], a2: => Fiber[E, A]) = a1.zipWith(a2)(A.append(_, _))
      final def empty = new Fiber[E, A] {
        def join: IO[E, A]                            = IO.now(A.empty)
        def interrupt[E2](t: Throwable): IO[E2, Unit] = IO.unit[E2]
      }
    })

  implicit def fiberApplicative[E]: Applicative[Fiber[E, ?]] =
    instanceOf(new ApplicativeClass[Fiber[E, ?]] {
      final def pure[A](a: A): Fiber[E, A] = new Fiber[E, A] {
        def join: IO[E, A]                            = IO.point(a)
        def interrupt[E2](t: Throwable): IO[E2, Unit] = IO.unit[E2]
      }

      final def ap[A, B](fa: Fiber[E, A])(f: Fiber[E, A => B]): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B] = fa.join.flatMap(a => f.join.map(f => f(a)))

        def interrupt[E2](t: Throwable): IO[E2, Unit] = fa.interrupt(t) *> f.interrupt(t)
      }

      final def map[A, B](ma: Fiber[E, A])(f: A => B): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B]                            = ma.join.map(f)
        def interrupt[E2](t: Throwable): IO[E2, Unit] = ma.interrupt(t)
      }
    })
}
