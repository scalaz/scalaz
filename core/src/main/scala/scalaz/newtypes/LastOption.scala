package scalaz
package newtypes

import data.LazyOption

sealed trait LastOption[A] {
  val value: Option[A]
}

object LastOption extends LastOptions

trait LastOptions {
  implicit def LastOption_^*^[A]: ^*^[LastOption[A], Option[A]] =
    ^*^.^*^(_.value, b => new LastOption[A] {
      val value = b
    })

  implicit def LastOption_^**^ : ^**^[LastOption, Option] =
    new ^**^[LastOption, Option] {
      def unpack[A] = _.value
      def pack[A] = b => new LastOption[A] {
      val value = b
    }
  }

  implicit def OptionLastOption[A](o: Option[A]): LastOption[A] =
    implicitly[^*^[LastOption[A], Option[A]]].pack(o)

  implicit def LastOptionZero[A]: Zero[LastOption[A]] =
    Zero.zero(implicitly[^*^[LastOption[A], Option[A]]].pack(None))

  implicit def LastOptionSemigroup[A]: Semigroup[LastOption[A]] = new Semigroup[LastOption[A]] {
    def append(a1: LastOption[A], a2: => LastOption[A]) =
      implicitly[^*^[LastOption[A], Option[A]]].pack(a2.value orElse a1.value)
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
