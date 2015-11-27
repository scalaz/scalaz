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

  implicit val idInstance: Traverse1[Id] with Monad[Id] with BindRec[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Align[Id] with Cozip[Id] with Optional[Id] = Id.id

  private[scalaz] type Tagged[A, T] = {type Tag = T; type Self = A}

  /**
   * Tag a type `T` with `Tag`.
   *
   * The resulting type is used to discriminate between type class instances.
   *
   * @see [[scalaz.Tag]] and [[scalaz.Tags]]
   *
   * Credit to Miles Sabin for the idea.
   */
  type @@[T, Tag] = Tagged[T, Tag]

  /** A [[scalaz.NaturalTransformation]][F, G]. */
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
  /** A [[scalaz.NaturalTransformation]][G, F]. */
  type <~[+F[_], -G[_]] = NaturalTransformation[G, F]
  type ~~>[-F[_,_], +G[_,_]] = BiNaturalTransformation[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  type ∨[A, B] = A \/ B

  type |>=|[G[_], F[_]] = MonadPartialOrder[G, F]

  type ReaderT[F[_], E, A] = Kleisli[F, E, A]
  val ReaderT = Kleisli
  type =?>[E, A] = Kleisli[Option, E, A]
  type Reader[E, A] = ReaderT[Id, E, A]

  type Writer[W, A] = WriterT[Id, W, A]
  type Unwriter[W, A] = UnwriterT[Id, W, A]

  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = Kleisli[Id, E, A](f)
  }

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[Id, W, A] = WriterT[Id, W, A]((w, a))
  }

  object Unwriter {
    def apply[U, A](u: U, a: A): UnwriterT[Id, U, A] = UnwriterT[Id, U, A]((u, a))
  }

  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  type IndexedState[-S1, S2, A] = IndexedStateT[Id, S1, S2, A]
  /** A state transition, representing a function `S => (S, A)`. */
  type State[S, A] = StateT[Id, S, A]

  object StateT extends StateTInstances with StateTFunctions {
    def apply[F[_], S, A](f: S => F[(S, A)])(implicit F: Monad[F]): StateT[F, S, A] = IndexedStateT[F, S, S, A](f)
  }
  object IndexedState extends StateFunctions {
    def apply[S1, S2, A](f: S1 => (S2, A)): IndexedState[S1, S2, A] = IndexedStateT[Id, S1, S2, A](f)
  }
  object State extends StateFunctions {
    def apply[S, A](f: S => (S, A)): State[S, A] = StateT[Id, S, A](f)
  }

  type StoreT[F[_], A, B] = IndexedStoreT[F, A, A, B]
  type IndexedStore[I, A, B] = IndexedStoreT[Id, I, A, B]
  type Store[A, B] = StoreT[Id, A, B]
  // flipped
  type |-->[A, B] = Store[B, A]
  object StoreT extends StoreTInstances with StoreTFunctions {
    def apply[F[_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
      storeT(r)
  }
  object IndexedStore {
    def apply[I, A, B](f: A => B, i: I): IndexedStore[I, A, B] = IndexedStoreT.indexedStore(i)(f)
  }
  object Store {
    def apply[A, B](f: A => B, a: A): Store[A, B] = StoreT.store(a)(f)
  }

  type Traced[A, B] = TracedT[Id, A, B]
  def Traced[A, B](f: A => B): Traced[A, B] = TracedT[Id, A, B](f)

  type ReaderWriterStateT[F[_], -R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A]
  object ReaderWriterStateT extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[F[_], R, W, S, A](f: (R, S) => F[(W, A, S)]): ReaderWriterStateT[F, R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A] { (r: R, s: S) => f(r, s) }
  }
  type IndexedReaderWriterState[-R, W, -S1, S2, A] = IndexedReaderWriterStateT[Id, R, W, S1, S2, A]
  object IndexedReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S1, S2, A](f: (R, S1) => (W, A, S2)): IndexedReaderWriterState[R, W, S1, S2, A] = IndexedReaderWriterStateT[Id, R, W, S1, S2, A] { (r: R, s: S1) => f(r, s) }
  }
  type ReaderWriterState[-R, W, S, A] = ReaderWriterStateT[Id, R, W, S, A]
  object ReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S, A](f: (R, S) => (W, A, S)): ReaderWriterState[R, W, S, A] = IndexedReaderWriterStateT[Id, R, W, S, S, A] { (r: R, s: S) => f(r, s) }
  }
  type IRWST[F[_], -R, W, -S1, S2, A] = IndexedReaderWriterStateT[F, R, W, S1, S2, A]
  val IRWST: IndexedReaderWriterStateT.type = IndexedReaderWriterStateT
  type IRWS[-R, W, -S1, S2, A] = IndexedReaderWriterState[R, W, S1, S2, A]
  val IRWS: IndexedReaderWriterState.type = IndexedReaderWriterState
  type RWST[F[_], -R, W, S, A] = ReaderWriterStateT[F, R, W, S, A]
  val RWST: ReaderWriterStateT.type = ReaderWriterStateT
  type RWS[-R, W, S, A] = ReaderWriterState[R, W, S, A]
  val RWS: ReaderWriterState.type = ReaderWriterState

  type Alternative[F[_]] = ApplicativePlus[F]

  type NonEmptyIList[A] = OneAnd[IList,A]

  /**
   * An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
   *
   * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
   */
  type ValidationNel[E, +X] = Validation[NonEmptyList[E], X]

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
  /**
   * A lens that doesn't transform the type of the record.
   *
   * @see [[scalaz.@>]]
   */
  type Lens[A, B] = LensFamily[A, A, B, B]

  /**
   * @see [[scalaz.Lens]]
   */
  object Lens extends LensInstances with LensFunctions {
    def apply[A, B](r: A => Store[B, A]): Lens[A, B] =
      lens(r)
  }

  /** @see [[scalaz.Lens]] */
  type @>[A, B] = Lens[A, B]

  //
  // Partial Lens type aliases
  //
  /**
   * A partial lens that doesn't transform the type of the record.
   *
   * @see [[scalaz.@?>]]
   */
  type PLens[A, B] = PLensFamily[A, A, B, B]

  /**
   * @see [[scalaz.PLens]]
   */
  object PLens extends PLensInstances with PLensFunctions {
    def apply[A, B](r: A => Option[Store[B, A]]): PLens[A, B] =
      plens(r)
  }

  /** @see [[scalaz.PLens]] */
  type @?>[A, B] = PLens[A, B]

  type PIndexedStateT[F[_], -S1, S2, A] = IndexedStateT[F, S1, S2, Option[A]]
  type PStateT[F[_], S, A] = PIndexedStateT[F, S, S, A]

  type PIndexedState[-S1, S2, A] = PIndexedStateT[Id, S1, S2, A]
  type PState[S, A] = PStateT[Id, S, A]

  type IndexedConts[W[_], R, O, A] = IndexedContsT[W, Id, R, O, A]
  object IndexedConts extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, O, A](f: W[A => O] => R): IndexedConts[W, R, O, A] = IndexedContsT[W, Id, R, O, A](f)
  }
  type IndexedContT[M[_], R, O, A] = IndexedContsT[Id, M, R, O, A]
  object IndexedContT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[M[_], R, O, A](f: (A => M[O]) => M[R]): IndexedContT[M, R, O, A] = IndexedContsT[Id, M, R, O, A](f)
  }
  type IndexedCont[R, O, A] = IndexedContT[Id, R, O, A]
  object IndexedCont extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[R, O, A](f: (A => O) => R): IndexedCont[R, O, A] = IndexedContsT[Id, Id, R, O, A](f)
  }
  type ContsT[W[_], M[_], R, A] = IndexedContsT[W, M, R, R, A]
  object ContsT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], M[_], R, A](f: W[A => M[R]] => M[R]): ContsT[W, M, R, A] = IndexedContsT[W, M, R, R, A](f)
  }
  type Conts[W[_], R, A] = ContsT[W, Id, R, A]
  object Conts extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, A](f: W[A => R] => R): Conts[W, R, A] = IndexedContsT[W, Id, R, R, A](f)
  }
  type ContT[M[_], R, A] = ContsT[Id, M, R, A]
  object ContT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[M[_], R, A](f: (A => M[R]) => M[R]): ContT[M, R, A] = IndexedContsT[Id, M, R, R, A](f)
  }
  type Cont[R, A] = ContT[Id, R, A]
  object Cont extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[R, A](f: (A => R) => R): Cont[R, A] = IndexedContsT[Id, Id, R, R, A](f)
  }

  /** [[scalaz.Inject]][F, G] */
  type :<:[F[_], G[_]] = Inject[F, G]

  /** [[scalaz.Inject]][F, G] */
  type :≺:[F[_], G[_]] = Inject[F, G]

  type IMap[A, B] = ==>>[A, B]
  val IMap = ==>>

  type GlorifiedTuple[+A, +B] = A \/ B

  type Disjunction[+A, +B] = \/[A, B]
  val Disjunction = \/

  type DLeft[+A] = -\/[A]
  val DLeft = -\/

  type DRight[+B] = \/-[B]
  val DRight = \/-

  type DisjunctionT[F[_], A, B] = EitherT[F, A, B]
  val DisjunctionT = EitherT
}
