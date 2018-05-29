// Copyright (C) 2018 John A. De Goes. All rights reserved.

package scalaz

import scala.AnyRef

package object effect {
  type Error            = java.lang.Error
  type Exception        = java.lang.Exception
  type InternalError    = java.lang.InternalError
  type OutOfMemoryError = java.lang.OutOfMemoryError
  type RuntimeException = java.lang.RuntimeException
  type Throwable        = java.lang.Throwable

  implicit class IOVoidSyntax[A](val io: IO[Void, A]) extends AnyRef {
    def apply[E]: IO[E, A] = io.asInstanceOf[IO[E, A]]
  }

  type Task[A] = IO[Throwable, A]

  type Unexceptional[A] = IO[Void, A]

  type Canceler     = Throwable => Unit
  type PureCanceler = Throwable => IO[Void, Unit]
}
