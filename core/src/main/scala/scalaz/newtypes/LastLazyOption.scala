package scalaz
package newtypes

import data._

sealed trait LastLazyOption[A] {
  val value: LazyOption[A]
}

object LastLazyOption extends LastLazyOptions

trait LastLazyOptions {
  implicit def LastLazyOptionUnpack[A]: Unpack[LastLazyOption[A], LazyOption[A]] = new Unpack[LastLazyOption[A], LazyOption[A]] {
    val unpack = (_: LastLazyOption[A]).value
  }

  implicit def LastLazyOptionPack[A]: Pack[LastLazyOption[A], LazyOption[A]] = new Pack[LastLazyOption[A], LazyOption[A]] {
    val pack = (b: LazyOption[A]) => new LastLazyOption[A] {
      val value = b
    }
  }

  implicit def LastLazyOptionNewtype[A]: Newtype[LastLazyOption[A], LazyOption[A]] =
    Newtype.newtype

  implicit def LazyOptionLastLazyOption[A](o: LazyOption[A]): LastLazyOption[A] =
    implicitly[Pack[LastLazyOption[A], LazyOption[A]]].pack(o)

  implicit def LastLazyOptionZero[A]: Zero[LastLazyOption[A]] =
    Zero.zero(implicitly[Pack[LastLazyOption[A], LazyOption[A]]].pack(LazyOption.lazyNone))

  implicit def LastLazyOptionSemigroup[A]: Semigroup[LastLazyOption[A]] = new Semigroup[LastLazyOption[A]] {
    def append(a1: LastLazyOption[A], a2: => LastLazyOption[A]) =
      implicitly[Pack[LastLazyOption[A], LazyOption[A]]].pack(a2.value orElse a1.value)
  }

  implicit def LastLazyOptionMonoid[A]: Monoid[LastLazyOption[A]] =
    Monoid.monoid

  implicit def LastLazyOptionShow[A: Show]: Show[LastLazyOption[A]] =
    implicitly[Show[LastLazyOption[A]]] contramap ((_: LastLazyOption[A]).value)

  implicit def LastLazyOptionEqual[A: Equal]: Equal[LastLazyOption[A]] =
    implicitly[Equal[LastLazyOption[A]]] contramap ((_: LastLazyOption[A]).value)

  implicit def LastLazyOptionOrder[A: Order]: Order[LastLazyOption[A]] =
    implicitly[Order[LastLazyOption[A]]] contramap ((_: LastLazyOption[A]).value)

}
