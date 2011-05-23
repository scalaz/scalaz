package scalaz

import collection.generic.CanBuildFrom

trait Zero[A] {
  val zero: A
}

object Zero extends Zeros

trait Zeros extends ZerosLow {
  def zero[A](a: A): Zero[A] = new Zero[A] {
    val zero = a
  }

  implicit val UnitZero: Zero[Unit] =
    zero(())

  implicit val BooleanZero: Zero[Boolean] =
    zero(false)

  implicit val IntZero: Zero[Int] =
    zero(0)

  implicit val StringZero: Zero[String] =
    zero("")

  implicit def StreamZero[A]: Zero[Stream[A]] =
    zero(Stream.Empty)

  implicit def ListZero[A]: Zero[List[A]] =
    zero(Nil)

  implicit def OptionZero[A]: Zero[Option[A]] =
    zero(None)

  implicit def Tuple2Zero[A, B](implicit za: Zero[A], zb: Zero[B]): Zero[(A, B)] =
    zero(za.zero, zb.zero)

  implicit def Tuple3Zero[A, B, C](implicit za: Zero[A], zb: Zero[B], zc: Zero[C]): Zero[(A, B, C)] =
    zero(za.zero, zb.zero, zc.zero)

}

trait ZerosLow {
  implicit def TraversableZero[CC <: Traversable[_]](implicit cbf: CanBuildFrom[Nothing, Nothing, CC]): Zero[CC] =
    new Zero[CC] {
      val zero = cbf.apply.result
    }
}