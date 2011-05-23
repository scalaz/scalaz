package scalaz
package newtypes

sealed trait LastOption[A] extends Pimp[Option[A]]

object LastOption extends LastOptions

trait LastOptions {
  implicit def LastOptionUnpack[A]: Unpack[LastOption[A], Option[A]] = new Unpack[LastOption[A], Option[A]] {
    val unpack = (_: LastOption[A]).value
  }

  implicit def LastOptionPack[A]: Pack[LastOption[A], Option[A]] = new Pack[LastOption[A], Option[A]] {
    val pack = (b: Option[A]) => new LastOption[A] {
      val value = b
    }
  }

  implicit def LastOptionNewtype[A]: Newtype[LastOption[A], Option[A]] =
    Newtype.newtype

  implicit def OptionLastOption[A](o: Option[A]): LastOption[A] =
    implicitly[Pack[LastOption[A], Option[A]]].pack(o)

  implicit def LastOptionZero[A]: Zero[LastOption[A]] =
    Zero.zero(implicitly[Pack[LastOption[A], Option[A]]].pack(None))

  implicit def LastOptionSemigroup[A]: Semigroup[LastOption[A]] = new Semigroup[LastOption[A]] {
    def append(a1: LastOption[A], a2: => LastOption[A]) =
      implicitly[Pack[LastOption[A], Option[A]]].pack(a2.value orElse a1.value)
  }

  implicit def LastOptionMonoid[A]: Monoid[LastOption[A]] =
    Monoid.monoid

  implicit def LastOptionShow[A: Show]: Show[LastOption[A]] =
    implicitly[Show[Option[A]]] contramap ((_: LastOption[A]).value)

  implicit def LastOptionEqual[A: Equal]: Equal[LastOption[A]] =
    implicitly[Equal[Option[A]]] contramap ((_: LastOption[A]).value)

  implicit def LastOptionOrder[A: Order]: Order[LastOption[A]] =
    implicitly[Order[Option[A]]] contramap ((_: LastOption[A]).value)

}
