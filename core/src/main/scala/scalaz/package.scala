/**
 * '''Scalaz''': Type classes and pure functional data structures for Scala.
 *
 * This package, [[scalaz]], contains:
 *  - type class definitions
 *  - data structures
 *  - related functions
 *
 * Type class instances and other functions related to the Scala and Java standard library
 * are in scalaz.[[scalaz.std]]
 *
 * Implicit conversions and wrapper classes that provide a more convenient syntax for accessing
 * the functionality of the library are in scalaz.[[scalaz.syntax]].
 *
 * '''Type Classes Index'''
 *
 *  - [[scalaz.Semigroup]]
 *  - [[scalaz.Monoid]] extends [[scalaz.Semigroup]]
 *  - [[scalaz.Group]] extends [[scalaz.Monoid]]
 *  - [[scalaz.Equal]]
 *  - [[scalaz.Length]]
 *  - [[scalaz.Show]]
 *  - [[scalaz.Order]] extends [[scalaz.Equal]]
 *
 *  - [[scalaz.MetricSpace]]
 *  - [[scalaz.Empty]]
 *  - [[scalaz.Each]]
 *  - [[scalaz.Index]]
 *  - [[scalaz.Functor]]
 *  - [[scalaz.Pointed]] extends [[scalaz.Functor]]
 *  - [[scalaz.Contravariant]]
 *  - [[scalaz.CoPointed]] extends [[scalaz.Functor]]
 *  - [[scalaz.Apply]] extends [[scalaz.Functor]]
 *  - [[scalaz.Applicative]] extends [[scalaz.Apply]] with [[scalaz.Pointed]]
 *  - [[scalaz.Bind]] extends [[scalaz.Apply]]
 *  - [[scalaz.Monad]] extends [[scalaz.Applicative]] with [[scalaz.Bind]]
 *  - [[scalaz.CoJoin]]
 *  - [[scalaz.CoBind]]
 *  - [[scalaz.CoMonad]] extends [[scalaz.CoPointed]] with [[scalaz.CoJoin]] with [[scalaz.CoBind]]
 *  - [[scalaz.Plus]] extends [[scalaz.Functor]] with [[scalaz.Empty]]
 *  - [[scalaz.ApplicativePlus]] extends [[scalaz.Applicative]] with [[scalaz.Plus]]
 *  - [[scalaz.MonadPlus]] extends [[scalaz.Monad]] with [[scalaz.ApplicativePlus]]
 *  - [[scalaz.Foldable]]
 *  - [[scalaz.Traverse]] extends [[scalaz.Functor]] with [[scalaz.Foldable]]
 *
 *  - [[scalaz.BiFunctor]]
 *  - [[scalaz.BiTraverse]] extends [[scalaz.BiFunctor]]
 *  - [[scalaz.ArrId]]
 *  - [[scalaz.Arr]]
 *  - [[scalaz.Compose]]
 *  - [[scalaz.Category]] extends [[scalaz.ArrId]] with [[scalaz.Compose]]
 *  - [[scalaz.First]]
 *  - [[scalaz.Arrow]] extends [[scalaz.Category]] with [[scalaz.Arr]] with [[scalaz.First]]
 *
 */
package object scalaz {
  type Id[X] = X

  object Id extends IdInstances

  // TODO Review!
  type Identity[X] = Need[X]

  type Tagged[T] = {type Tag = T}

  /**
   * Tag a type `T` with `Tag`. The resulting type is a subtype of `T`.
   *
   * The resulting type is used to discriminate between type class instances.
   *
   * @see [[scalaz.Tag]] and [[scalaz.Tags]]
   *
   * Credit to Miles Sabin for the idea.
   */
  type @@[T, Tag] = T with Tagged[Tag]

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
  type <~[F[_], G[_]] = NaturalTransformation[G, F]
  type ~~>[F[_,_], G[_,_]] = BiNaturalTransformation[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  /** A state transition, representing a function `S => (A, S)`. */
  type State[S, A] = StateT[Id, S, A]

  object State extends StateFunctions {
    def apply[S, A](f: S => (A, S)): State[S, A] = new StateT[Id, S, A] {
      def apply(s: S) = f(s)
    }
  }

  /**
   * An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
   *
   * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
   */
  type ValidationNEL[E, X] = Validation[NonEmptyList[E], X]
}