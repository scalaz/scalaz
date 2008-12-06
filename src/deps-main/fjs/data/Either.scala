package fjs.data

import fj.data.Either.{left, right}

object Either {
  implicit def ScalaEither_Either[A, B](e: scala.Either[A, B]): fj.data.Either[A, B] = e match {
    case scala.Left(a) => left(a)
    case scala.Right(b) => right(b)
  }

  implicit def Either_ScalaEither[A, B](e: fj.data.Either[A, B]) =
    if(e.isLeft) scala.Left(e.left.value)
    else scala.Right(e.right.value)

  object Left {
    def unapply[A, B](e: fj.data.Either[A, B]) =
      if(e.isLeft) Some(e.left.value)
      else None
  }

  object Right {
    def unapply[A, B](e: fj.data.Either[A, B]) =
      if(e.isRight) Some(e.right.value)
      else None
  }
}
