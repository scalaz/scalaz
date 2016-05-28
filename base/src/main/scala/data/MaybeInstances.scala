package scalaz
package data

import clazz._
import Maybe.{Just, Empty}

trait MaybeInstances extends MonadClass[Maybe] {
  override def ap[A, B](ma: Maybe[A])(mf: Maybe[A => B]): Maybe[B] =
    ma.fold(a => map[A => B, B](mf)(f => f(a)), Empty())

  override def flatMap[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
    ma.fold(a => f(a), Empty())

  override def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
    ma.fold(a => Just(f(a)), Empty())

  override def pure[A](a: A): Maybe[A] =
    Just(a)
}
