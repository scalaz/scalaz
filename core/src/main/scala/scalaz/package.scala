/**
 * '''Scalaz''': Type classes and pure functional data structures for Scala.
 *
 * This package, [[scalaz]], contains:
 *  - type class definitions
 *  - data structures
 *  - related functions
 *
 * Type class instances and other functions related to the Scala and Java standard library
 * are in [[scalaz.std]]
 *
 * Implicit conversions and wrapper classes that provide a more convenient syntax for accessing
 * the functionality of the library are in [[scalaz.syntax]].
 *
 * '''Type Classes Index'''
 *
 *  - [[scalaz.Semigroup]]
 *  - [[scalaz.Monoid]] extends [[scalaz.Semigroup]]
 *  - [[scalaz.Equal]]
 *  - [[scalaz.Show]]
 *  - [[scalaz.Order]] extends [[scalaz.Equal]]
 *  - [[scalaz.Enum]] extends [[scalaz.Order]]
 *
 *  - [[scalaz.Plus]]
 *  - [[scalaz.PlusEmpty]] extends [[scalaz.Plus]]
 *  - [[scalaz.IsEmpty]] extends [[scalaz.PlusEmpty]]
 *  - [[scalaz.Optional]]
 *  - [[scalaz.InvariantFunctor]]
 *  - [[scalaz.Functor]] extends [[scalaz.InvariantFunctor]]
 *  - [[scalaz.Contravariant]] extends [[scalaz.InvariantFunctor]]
 *  - [[scalaz.Divide]] extends [[scalaz.Contravariant]]
 *  - [[scalaz.Divisible]] extends [[scalaz.Divide]]
 *  - [[scalaz.Apply]] extends [[scalaz.Functor]]
 *  - [[scalaz.Applicative]] extends [[scalaz.Apply]]
 *  - [[scalaz.Align]] extends [[scalaz.Functor]]
 *  - [[scalaz.Zip]]
 *  - [[scalaz.Unzip]]
 *  - [[scalaz.Cozip]]
 *  - [[scalaz.Bind]] extends [[scalaz.Apply]]
 *  - [[scalaz.BindRec]] extends [[scalaz.Bind]]
 *  - [[scalaz.Monad]] extends [[scalaz.Applicative]] with [[scalaz.Bind]]
 *  - [[scalaz.Cobind]] extends [[scalaz.Functor]]
 *  - [[scalaz.Comonad]] extends [[scalaz.Cobind]]
 *  - [[scalaz.ApplicativePlus]] extends [[scalaz.Applicative]] with [[scalaz.PlusEmpty]]
 *  - [[scalaz.MonadPlus]] extends [[scalaz.Monad]] with [[scalaz.ApplicativePlus]]
 *  - [[scalaz.Foldable]]
 *  - [[scalaz.Foldable1]] extends [[scalaz.Foldable]]
 *  - [[scalaz.Traverse]] extends [[scalaz.Functor]] with [[scalaz.Foldable]]
 *  - [[scalaz.Traverse1]] extends [[scalaz.Traverse]] with [[scalaz.Foldable1]]
 *
 *  - [[scalaz.Associative]]
 *  - [[scalaz.Bifunctor]]
 *  - [[scalaz.Bifoldable]]
 *  - [[scalaz.Bitraverse]] extends [[scalaz.Bifunctor]] with [[scalaz.Bifoldable]]
 *  - [[scalaz.Catchable]]
 *  - [[scalaz.Nondeterminism]] extends [[scalaz.Monad]]
 *  - [[scalaz.Compose]]
 *  - [[scalaz.Profunctor]]
 *  - [[scalaz.Strong]] extends [[scalaz.Profunctor]]
 *  - [[scalaz.ProChoice]] extends [[scalaz.Profunctor]]
 *  - [[scalaz.Category]] extends [[scalaz.Compose]]
 *  - [[scalaz.Choice]] extends [[scalaz.Category]]
 *  - [[scalaz.Split]] extends [[scalaz.Compose]]
 *  - [[scalaz.Arrow]] extends [[scalaz.Split]] with [[scalaz.Strong]] with [[scalaz.Category]]
 *  - [[scalaz.MonadState]] extends [[scalaz.Monad]]
 *  - [[scalaz.MonadError]] extends [[scalaz.Monad]]
 *  - [[scalaz.MonadTell]] extends [[scalaz.Monad]]
 *  - [[scalaz.MonadReader]] extends [[scalaz.Monad]]
 *  - [[scalaz.ComonadStore]] extends [[scalaz.Comonad]]
 *
 *  '''Data Structures Index'''
 *  - [[scalaz.Validation]] Represent computations that may succeed or fail, accumulating multiple errors.
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
 *  - [[scalaz.MaybeT]] Represents computations of type `F[Maybe[A]]`
 *  - [[scalaz.EitherT]] Represents computations of type `F[A \/ B]`
 */
package object scalaz {

  import Id._

  val idInstance: Traverse1[Id] with Monad[Id] with BindRec[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Align[Id] with Cozip[Id] with Optional[Id] = Id.id

  /**
   * Tag a type `T` with `Tag`.
   *
   * The resulting type is used to discriminate between type class instances.
   *
   * @see [[scalaz.Tag]] and [[scalaz.Tags]]
   *
   * Credit to Miles Sabin for the idea.
   */
  type @@[T, Tag] = scalaz.Tag.k.@@[T, Tag]

  /** A [[scalaz.NaturalTransformation]][F, G]. */
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]
  /** A [[scalaz.NaturalTransformation]][G, F]. */
  type <~[F[_], G[_]] = NaturalTransformation[G, F]
  type ~~>[F[_,_], G[_,_]] = BiNaturalTransformation[F, G]

  /** `(A === B)` is a supertype of `Leibniz[L,H,A,B]` */
  type ===[A,B] = Leibniz[⊥, ⊤, A, B]

  type ⊥ = Nothing
  type ⊤ = Any

  type ∨[A, B] = A \/ B

  type ReaderT[E, F[_], A] = Kleisli[F, E, A]
  type =?>[E, A] = Kleisli[Option, E, A]

  /** @template */
  type Reader[E, A] = ReaderT[E, Id, A]

  /** @template */
  type Writer[W, A] = WriterT[W, Id, A]

  /** @template */
  type Unwriter[W, A] = UnwriterT[Id, W, A]

  /** @template */
  type StateT[S, F[_], A] = IndexedStateT[S, S, F, A]

  /** @template */
  type IndexedState[S1, S2, A] = IndexedStateT[S1, S2, Id, A]

  /** A state transition, representing a function `S => (S, A)`.
    *
    * @template
    */
  type State[S, A] = StateT[S, Id, A]

  /** @template */
  type StoreT[F[_], A, B] = IndexedStoreT[F, A, A, B]

  /** @template */
  type IndexedStore[I, A, B] = IndexedStoreT[Id, I, A, B]

  /** @template */
  type Store[A, B] = StoreT[Id, A, B]
  // flipped
  type |-->[A, B] = Store[B, A]

  /** @template */
  type Traced[A, B] = TracedT[Id, A, B]
  def Traced[A, B](f: A => B): Traced[A, B] = TracedT[Id, A, B](f)

  /** @template */
  type ReaderWriterStateT[R, W, S, F[_], A] = IndexedReaderWriterStateT[R, W, S, S, F, A]

  /** @template */
  type IndexedReaderWriterState[R, W, S1, S2, A] = IndexedReaderWriterStateT[R, W, S1, S2, Id, A]

  /** @template */
  type ReaderWriterState[R, W, S, A] = ReaderWriterStateT[R, W, S, Id, A]

  type IRWST[R, W, S1, S2, F[_], A] = IndexedReaderWriterStateT[R, W, S1, S2, F, A]
  val IRWST: IndexedReaderWriterStateT.type = IndexedReaderWriterStateT
  type IRWS[R, W, S1, S2, A] = IndexedReaderWriterState[R, W, S1, S2, A]
  val IRWS: IndexedReaderWriterState.type = IndexedReaderWriterState
  type RWST[R, W, S, F[_], A] = ReaderWriterStateT[R, W, S, F, A]
  val RWST: ReaderWriterStateT.type = ReaderWriterStateT
  type RWS[R, W, S, A] = ReaderWriterState[R, W, S, A]
  val RWS: ReaderWriterState.type = ReaderWriterState

  /** An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
    *
    * @template
    *
    * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
    */
  type ValidationNel[E, X] = Validation[NonEmptyList[E], X]

  type FirstOf[A] = A @@ Tags.FirstVal
  type LastOf[A] = A @@ Tags.LastVal
  type MinOf[A] = A @@ Tags.MinVal
  type MaxOf[A] = A @@ Tags.MaxVal

  type FirstOption[A] = Option[A] @@ Tags.First
  type LastOption[A] = Option[A] @@ Tags.Last
  type MinOption[A] = Option[A] @@ Tags.Min
  type MaxOption[A] = Option[A] @@ Tags.Max

  type FirstMaybe[A] = Maybe[A] @@ Tags.First
  type LastMaybe[A] = Maybe[A] @@ Tags.Last
  type MinMaybe[A] = Maybe[A] @@ Tags.Min
  type MaxMaybe[A] = Maybe[A] @@ Tags.Max

  //
  // Lens type aliases
  //
  /** A lens that doesn't transform the type of the record.
    *
    * @template
    * @see [[scalaz.@>]]
    */
  type Lens[A, B] = LensFamily[A, A, B, B]

  /** @see [[scalaz.Lens]] */
  type @>[A, B] = Lens[A, B]

  //
  // Partial Lens type aliases
  //
  /** A partial lens that doesn't transform the type of the record.
    *
    * @template
    * @see [[scalaz.@?>]]
    */
  type PLens[A, B] = PLensFamily[A, A, B, B]

  /** @see [[scalaz.PLens]] */
  type @?>[A, B] = PLens[A, B]

  /** @template */
  type PIndexedStateT[F[_], S1, S2, A] = IndexedStateT[S1, S2, F, Option[A]]

  /** @template */
  type PStateT[F[_], S, A] = PIndexedStateT[F, S, S, A]

  /** @template */
  type PIndexedState[S1, S2, A] = PIndexedStateT[Id, S1, S2, A]

  /** @template */
  type PState[S, A] = PStateT[Id, S, A]

  /** @template */
  type IndexedConts[W[_], R, O, A] = IndexedContsT[W, R, O, Id, A]

  /** @template */
  type IndexedContT[R, O, M[_], A] = IndexedContsT[Id, R, O, M, A]

  /** @template */
  type IndexedCont[R, O, A] = IndexedContT[R, O, Id, A]

  /** @template */
  type ContsT[W[_], R, M[_], A] = IndexedContsT[W, R, R, M, A]

  /** @template */
  type Conts[W[_], R, A] = ContsT[W, R, Id, A]

  /** @template */
  type ContT[R, M[_], A] = ContsT[Id, R, M, A]

  /** @template */
  type Cont[R, A] = ContT[R, Id, A]

  /** @template */
  type Select[R, A] = SelectT[R, Id, A]

  /** [[scalaz.Inject]][F, G] */
  type :<:[F[_], G[_]] = Inject[F, G]

  /** [[scalaz.Inject]][F, G] */
  type :≺:[F[_], G[_]] = Inject[F, G]

  type IMap[A, B] = ==>>[A, B]
  val IMap = ==>>

  type Disjunction[A, B] = \/[A, B]
  val Disjunction = \/

  type DLeft[A, B] = -\/[A, B]
  val DLeft = -\/

  type DRight[A, B] = \/-[A, B]
  val DRight = \/-

  type DisjunctionT[A, F[_], B] = EitherT[A, F, B]
  val DisjunctionT = EitherT

  type Pair[A] = (A, A)
}
