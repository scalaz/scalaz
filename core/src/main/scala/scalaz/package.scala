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
 *  - [[scalaz.Compose]]
 *  - [[scalaz.Category]] extends [[scalaz.ArrId]] with [[scalaz.Compose]]
 *  - [[scalaz.Arrow]] extends [[scalaz.Category]]
 *
 *  '''Data Structures Index'''
 *  - [[scalaz.Validation]] Represent computations that may success or fail, accumulating multiple errors.
 *  - [[scalaz.NonEmptyList]] A list containing at least one element.
 *  - [[scalaz.DList]] A difference list, supporting efficient append and prepend.
 *  - [[scalaz.EphemeralStream]] A stream that holds weak references to its elements, and recomputes them if needed
 *    if reclaimed by the garbage collector.
 *  - [[scalaz.Heap]] A priority queue, implemented with bootstrapped skew binomial heaps.
 *  - [[scalaz.Endo]] Represents functions from `A => A`.
 *  - [[scalaz.FingerTree]] A tree containing elements at it's leaves, and measures at the nodes. Can be adapted to
 *    various purposes by choosing a different measure, for example [[scalaz.IndSeq]] and [[scalaz.OrdSeq]].
 *  - [[scalaz.Lens]] Composable, functional alternative to getters and setters
 *  - [[scalaz.Tree]] A multiway tree. Each node contains a single element, and a `Stream` of sub-trees.
 *  - [[scalaz.TreeLoc]] A cursor over a [[scalaz.Tree]].
 *  - [[scalaz.Zipper]] A functional cursor over a List.
 *
 *  - [[scalaz.Kleisli]] Represents a function `A => M[B]`, allowing chaining. Also known, and aliased, as `scalaz.ReaderT`.
 *  - [[scalaz.StateT]] Computations that modify state.
 *  - [[scalaz.WriterT]] Computations that log a value
 *  - [[scalaz.OptionT]] Represents computations of type `F[Option[A]]`
 *  - [[scalaz.EitherT]] Represents computations of type `F[Either[A, B]]`
 */
package object scalaz {
  /** The strict identity type constructor. Can be thought of as `Tuple1`, but with no
   *  runtime representation.
   */
  type Id[X] = X

  /**
   * Type class instance for the strict identity type constructor
   *
   * This is important when using aliases like `State[A, B]`, which is a type alias for
   * `StateT[Id, A, B]`.
   */
  // WARNING: Don't mix this instance in via a trait. https://issues.scala-lang.org/browse/SI-5268
  implicit val idInstance = Id.id

  object Id extends IdInstances {
  }

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

  type ReaderT[F[_], E, A] = Kleisli[F, E, A]
  type Reader[E, A] = ReaderT[Id, E, A]

  type Writer[W, A] = WriterT[Id, W, A]

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[Id, W, A] = WriterT[Id, W, A]((w, a))
  }

  /** A state transition, representing a function `S => (A, S)`. */
  type State[S, A] = StateT[Id, S, A]

  object State extends StateFunctions {
    def apply[S, A](f: S => (A, S)): State[S, A] = new StateT[Id, S, A] {
      def apply(s: S) = f(s)
    }
  }

  type ReaderWriterState[R, W, S, A] = ReaderWriterStateT[Identity, R, W, S, A]

  type RWST[F[_], R, W, S, A] = ReaderWriterStateT[F, R, W, S, A]

  val RWST: ReaderWriterStateT.type = ReaderWriterStateT

  type RWS[R, W, S, A] = ReaderWriterState[R, W, S, A]

  /**
   * An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
   *
   * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
   */
  type ValidationNEL[E, X] = Validation[NonEmptyList[E], X]

  type FirstOption[A] = Option[A] @@ Tags.First
  type LastOption[A] = Option[A] @@ Tags.Last
}