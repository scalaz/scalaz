package scalaz
package newtypes

import data.LazyOption

sealed trait LastOption[A] {
  val value: Option[A]
}

object LastOption extends LastOptions

trait LastOptions {
  implicit def LastOption_^*^[A]: (LastOption[A] ^*^ Option[A]) =
    ^*^.^*^(_.value, b => new LastOption[A] {
      val value = b
    })

  implicit def LastOption_^**^ : (LastOption ^**^ Option) =
    new (LastOption ^**^ Option) {
      def unpack[A] = _.value

      def pack[A] = b => new LastOption[A] {
        val value = b
      }
    }

  implicit def OptionLastOption[A](o: Option[A]): LastOption[A] =
    implicitly[LastOption[A] ^*^ Option[A]].pack(o)

  implicit def LastOptionZero[A]: Zero[LastOption[A]] =
    Zero.zero(implicitly[LastOption[A] ^*^ Option[A]].pack(None))

  implicit def LastOptionSemigroup[A]: Semigroup[LastOption[A]] = new Semigroup[LastOption[A]] {
    def append(a1: LastOption[A], a2: => LastOption[A]) =
      implicitly[LastOption[A] ^*^ Option[A]].pack(a2.value orElse a1.value)
  }

  implicit def LastOptionMonoid[A]: Monoid[LastOption[A]] =
    Monoid.monoid

  implicit def LastOptionShow[A: Show]: Show[LastOption[A]] =
    implicitly[Show[Option[A]]] contramap ((_: LastOption[A]).value)

  implicit def LastOptionEqual[A: Equal]: Equal[LastOption[A]] =
    implicitly[Equal[Option[A]]] contramap ((_: LastOption[A]).value)

  implicit def LastOptionOrder[A: Order]: Order[LastOption[A]] =
    implicitly[Order[Option[A]]] contramap ((_: LastOption[A]).value)
 
  implicit def LastOptionPointed: Pointed[LastOption] =
    implicitly[Pointed[Option]].deriving[LastOption]

  implicit def LastOptionFunctor: Functor[LastOption] =
    implicitly[Functor[Option]].deriving[LastOption]

  implicit def LastOptionApplic: Applic[LastOption] =
    implicitly[Applic[Option]].deriving[LastOption]

  implicit def LastOptionApplicative: Applicative[LastOption] =
    implicitly[Applicative[Option]].deriving[LastOption]

  implicit def LastOptionApplicFunctor: ApplicFunctor[LastOption] =
    implicitly[ApplicFunctor[Option]].deriving[LastOption]

  implicit def LastOptionBind: Bind[LastOption] =
    implicitly[Bind[Option]].deriving[LastOption]

  implicit def LastOptionBindFunctor: BindFunctor[LastOption] =
    implicitly[BindFunctor[Option]].deriving[LastOption]

  implicit def LastOptionEach: Each[LastOption] =
    implicitly[Each[Option]].deriving[LastOption]

  implicit def LastOptionFoldr: Foldr[LastOption] =
    implicitly[Foldr[Option]].deriving[LastOption]

  implicit def LastOptionFoldl: Foldl[LastOption] =
    implicitly[Foldl[Option]].deriving[LastOption]

  implicit def LastOptionFoldable: Foldable[LastOption] =
    implicitly[Foldable[Option]].deriving[LastOption]

  implicit def LastOptionIndex: Index[LastOption] =
    implicitly[Index[Option]].deriving[LastOption]

  implicit def LastOptionLength: Length[LastOption] =
    implicitly[Length[Option]].deriving[LastOption]

  implicit def LastOptionMonad: Monad[LastOption] =
    implicitly[Monad[Option]].deriving[LastOption]

  implicit def LastOptionMonadEmpty: MonadEmpty[LastOption] =
    implicitly[MonadEmpty[Option]].deriving[LastOption]

  implicit def LastOptionMonadEmptyPlus: MonadEmptyPlus[LastOption] =
    implicitly[MonadEmptyPlus[Option]].deriving[LastOption]

  implicit def LastOptionPlus: Plus[LastOption] =
    implicitly[Plus[Option]].deriving[LastOption]

  implicit def LastOptionPointedEmpty: PointedEmpty[LastOption] =
    implicitly[PointedEmpty[Option]].deriving[LastOption]

  implicit def LastOptionPointedFunctor: PointedFunctor[LastOption] =
    implicitly[PointedFunctor[Option]].deriving[LastOption]

  implicit def LastOptionTraverse: Traverse[LastOption] =
    implicitly[Traverse[Option]].deriving[LastOption]
}
