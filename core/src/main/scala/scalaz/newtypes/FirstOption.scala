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

  implicit def FirstOptionPointed: Pointed[FirstOption] =
    implicitly[Pointed[Option]].deriving[FirstOption]

  implicit def FirstOptionFunctor: Functor[FirstOption] =
    implicitly[Functor[Option]].deriving[FirstOption]

  implicit def FirstOptionApplic: Applic[FirstOption] =
    implicitly[Applic[Option]].deriving[FirstOption]

  implicit def FirstOptionApplicative: Applicative[FirstOption] =
    implicitly[Applicative[Option]].deriving[FirstOption]

  implicit def FirstOptionApplicFunctor: ApplicFunctor[FirstOption] =
    implicitly[ApplicFunctor[Option]].deriving[FirstOption]

  implicit def FirstOptionBind: Bind[FirstOption] =
    implicitly[Bind[Option]].deriving[FirstOption]

  implicit def FirstOptionBindFunctor: BindFunctor[FirstOption] =
    implicitly[BindFunctor[Option]].deriving[FirstOption]

  implicit def FirstOptionEach: Each[FirstOption] =
    implicitly[Each[Option]].deriving[FirstOption]

  implicit def FirstOptionFoldr: Foldr[FirstOption] =
    implicitly[Foldr[Option]].deriving[FirstOption]

  implicit def FirstOptionFoldl: Foldl[FirstOption] =
    implicitly[Foldl[Option]].deriving[FirstOption]

  implicit def FirstOptionFoldable: Foldable[FirstOption] =
    implicitly[Foldable[Option]].deriving[FirstOption]

  implicit def FirstOptionIndex: Index[FirstOption] =
    implicitly[Index[Option]].deriving[FirstOption]

  implicit def FirstOptionLength: Length[FirstOption] =
    implicitly[Length[Option]].deriving[FirstOption]

  implicit def FirstOptionMonad: Monad[FirstOption] =
    implicitly[Monad[Option]].deriving[FirstOption]

  implicit def FirstOptionMonadEmpty: MonadEmpty[FirstOption] =
    implicitly[MonadEmpty[Option]].deriving[FirstOption]

  implicit def FirstOptionMonadEmptyPlus: MonadEmptyPlus[FirstOption] =
    implicitly[MonadEmptyPlus[Option]].deriving[FirstOption]

  implicit def FirstOptionPlus: Plus[FirstOption] =
    implicitly[Plus[Option]].deriving[FirstOption]

  implicit def FirstOptionPointedEmpty: PointedEmpty[FirstOption] =
    implicitly[PointedEmpty[Option]].deriving[FirstOption]

  implicit def FirstOptionPointedFunctor: PointedFunctor[FirstOption] =
    implicitly[PointedFunctor[Option]].deriving[FirstOption]

  implicit def FirstOptionTraverse: Traverse[FirstOption] =
    implicitly[Traverse[Option]].deriving[FirstOption]

}
