package scalaz
package newtypes

sealed trait FirstOption[A] {
  val value: Option[A]
}

object FirstOption extends FirstOptions

trait FirstOptions {
  implicit def FirstOption_^*^[A]: ^*^[FirstOption[A], Option[A]] =
    ^*^.^*^(_.value, b => new FirstOption[A] {
      val value = b
    })

  implicit def FirstOption_^**^ : ^**^[FirstOption, Option] =
    new ^**^[FirstOption, Option] {
      def unpack[A] = _.value
      def pack[A] = b => new FirstOption[A] {
      val value = b
    }
  }

  implicit def OptionFirstOption[A](o: Option[A]): FirstOption[A] =
    implicitly[^*^[FirstOption[A], Option[A]]].pack(o)

  implicit def FirstOptionZero[A]: Zero[FirstOption[A]] =
    Zero.zero(implicitly[^*^[FirstOption[A], Option[A]]].pack(None))

  implicit def FirstOptionSemigroup[A]: Semigroup[FirstOption[A]] = new Semigroup[FirstOption[A]] {
    def append(a1: FirstOption[A], a2: => FirstOption[A]) =
      implicitly[^*^[FirstOption[A], Option[A]]].pack(a1.value orElse a2.value)
  }

  implicit def FirstOptionMonoid[A]: Monoid[FirstOption[A]] =
    Monoid.monoid

  implicit def FirstOptionShow[A: Show]: Show[FirstOption[A]] =
    implicitly[Show[Option[A]]] contramap ((_: FirstOption[A]).value)

  implicit def FirstOptionEqual[A: Equal]: Equal[FirstOption[A]] =
    implicitly[Equal[Option[A]]] contramap ((_: FirstOption[A]).value)

  implicit def FirstOptionOrder[A: Order]: Order[FirstOption[A]] =
    implicitly[Order[Option[A]]] contramap ((_: FirstOption[A]).value)

  implicit def FirstOptionPointed: Pointed[FirstOption] = new Pointed[FirstOption] {
    def point[A](a: => A) = implicitly[^*^[FirstOption[A], Option[A]]].pack(Some(a))
  }

}
