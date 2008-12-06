package fjs.data

import fj.data.Option.{some, none}

object Option {
  implicit def ScalaOption_Option[A](o: scala.Option[A]): fj.data.Option[A] = o match {
    case scala.Some(a) => some(a)
    case scala.None => none[A]
  }

  implicit def Option_ScalaOption[A](o: fj.data.Option[A]) =
    if(o.isSome) scala.Some(o.some)
    else scala.None

  object Some {
    def unapply[A](o: fj.data.Option[A]) = Option_ScalaOption(o)
  }
}
