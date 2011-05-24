package scalaz
package newtypes

import data._

sealed trait LastLazyOption[A] {
  val value: LazyOption[A]
}

object LastLazyOption extends LastLazyOptions

trait LastLazyOptions {
  implicit def LastLazyOptionNewtype[A]: ^*^[LastLazyOption[A], LazyOption[A]] =
    ^*^.^*^(_.value, b => new LastLazyOption[A] {
      val value = b
    })


  implicit def LazyOptionLastLazyOption[A](o: LazyOption[A]): LastLazyOption[A] =
    implicitly[^*^[LastLazyOption[A], LazyOption[A]]].pack(o)

  implicit def LastLazyOptionZero[A]: Zero[LastLazyOption[A]] =
    Zero.zero(implicitly[^*^[LastLazyOption[A], LazyOption[A]]].pack(LazyOption.lazyNone))

  implicit def LastLazyOptionSemigroup[A]: Semigroup[LastLazyOption[A]] = new Semigroup[LastLazyOption[A]] {
    def append(a1: LastLazyOption[A], a2: => LastLazyOption[A]) =
      implicitly[^*^[LastLazyOption[A], LazyOption[A]]].pack(a2.value orElse a1.value)
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
