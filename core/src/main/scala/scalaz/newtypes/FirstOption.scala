package scalaz
package newtypes

sealed trait FirstOption[A] {
  val value: Option[A]
}

object FirstOption extends FirstOptions

trait FirstOptions {
  implicit def FirstOptionUnpack[A]: Unpack[FirstOption[A], Option[A]] = new Unpack[FirstOption[A], Option[A]] {
    val unpack = (_: FirstOption[A]).value
  }

  implicit def FirstOptionPack[A]: Pack[FirstOption[A], Option[A]] = new Pack[FirstOption[A], Option[A]] {
    val pack = (b: Option[A]) => new FirstOption[A] {
      val value = b
    }
  }

  implicit def FirstOptionNewtype[A]: Newtype[FirstOption[A], Option[A]] =
    Newtype.newtype

  implicit def OptionFirstOption[A](o: Option[A]): FirstOption[A] =
    implicitly[Pack[FirstOption[A], Option[A]]].pack(o)

  implicit def FirstOptionZero[A]: Zero[FirstOption[A]] =
    Zero.zero(implicitly[Pack[FirstOption[A], Option[A]]].pack(None))

  implicit def FirstOptionSemigroup[A]: Semigroup[FirstOption[A]] = new Semigroup[FirstOption[A]] {
    def append(a1: FirstOption[A], a2: => FirstOption[A]) =
      implicitly[Pack[FirstOption[A], Option[A]]].pack(a1.value orElse a2.value)
  }

  implicit def FirstOptionMonoid[A]: Monoid[FirstOption[A]] =
    Monoid.monoid

  implicit def FirstOptionShow[A: Show]: Show[FirstOption[A]] =
    implicitly[Show[Option[A]]] contramap ((_: FirstOption[A]).value)

  implicit def FirstOptionEqual[A: Equal]: Equal[FirstOption[A]] =
    implicitly[Equal[Option[A]]] contramap ((_: FirstOption[A]).value)

  implicit def FirstOptionOrder[A: Order]: Order[FirstOption[A]] =
    implicitly[Order[Option[A]]] contramap ((_: FirstOption[A]).value)

}
