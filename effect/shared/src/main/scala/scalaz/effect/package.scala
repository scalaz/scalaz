// Copyright (C) 2018 John A. De Goes. All rights reserved.

package scalaz

package object effect {
  type Task[A] = IO[Throwable, A]

  type Unexceptional[A] = IO[Void, A]
}
