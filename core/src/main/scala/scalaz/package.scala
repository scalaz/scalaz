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
 *  - [[scalaz.Equal]]
 *  - [[scalaz.Length]]
 *  - [[scalaz.Show]]
 *  - [[scalaz.Order]] extends [[scalaz.Equal]]
 *  - [[scalaz.Enum]] extends [[scalaz.Order]]
 *
 *  - [[scalaz.MetricSpace]]
 *  - [[scalaz.Plus]]
 *  - [[scalaz.PlusEmpty]] extends [[scalaz.Plus]]
 *  - [[scalaz.IsEmpty]] extends [[scalaz.PlusEmpty]]
 *  - [[scalaz.Each]]
 *  - [[scalaz.Index]]
 *  - [[scalaz.InvariantFunctor]]
 *  - [[scalaz.Functor]] extends [[scalaz.InvariantFunctor]]
 *  - [[scalaz.Contravariant]] extends [[scalaz.InvariantFunctor]]
 *  - [[scalaz.Apply]] extends [[scalaz.Functor]]
 *  - [[scalaz.Applicative]] extends [[scalaz.Apply]]
 *  - [[scalaz.Zip]]
 *  - [[scalaz.Unzip]]
 *  - [[scalaz.Cozip]]
 *  - [[scalaz.Bind]] extends [[scalaz.Apply]]
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
 *  - [[scalaz.Bifunctor]]
 *  - [[scalaz.Bifoldable]]
 *  - [[scalaz.Bitraverse]] extends [[scalaz.Bifunctor]] with [[scalaz.Bifoldable]]
 *  - [[scalaz.Catchable]]
 *  - [[scalaz.Nondeterminism]] extends [[scalaz.Monad]]
 *  - [[scalaz.Compose]]
 *  - [[scalaz.Profunctor]]
 *  - [[scalaz.Category]] extends [[scalaz.Compose]]
 *  - [[scalaz.Choice]] extends [[scalaz.Category]]
 *  - [[scalaz.Split]] extends [[scalaz.Compose]]
 *  - [[scalaz.Arrow]] extends [[scalaz.Split]] with [[scalaz.Profunctor]] with [[scalaz.Category]]
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
 *  - [[scalaz.EitherT]] Represents computations of type `F[A \/ B]`
 */
package object scalaz {
  import Id._

  implicit val idInstance: Traverse1[Id] with Each[Id] with Monad[Id] with Comonad[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Cozip[Id] = Id.id

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
  type @@[+T, Tag] = T with Tagged[Tag]

  /** A [[scalaz.NaturalTransformation]][F, G]. */
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
  /** A [[scalaz.NaturalTransformation]][G, F]. */
  type <~[+F[_], -G[_]] = NaturalTransformation[G, F]
  type ~~>[-F[_,_], +G[_,_]] = BiNaturalTransformation[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  type |>=|[G[_], F[_]] = MonadPartialOrder[G, F]

  type ReaderT[F[_], E, A] = Kleisli[F, E, A]
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
  /** A state transition, representing a function `S => (A, S)`. */
  type State[S, A] = StateT[Id, S, A]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object StateT extends StateTFunctions with StateTInstances {
    def apply[F[_], S, A](f: S => F[(S, A)]): StateT[F, S, A] = new StateT[F, S, A] {
      def apply(s: S) = f(s)
    }
  }
  object IndexedState extends StateFunctions {
    def apply[S1, S2, A](f: S1 => (S2, A)): IndexedState[S1, S2, A] = new IndexedState[S1, S2, A] {
      def apply(s: S1) = f(s)
    }
  }
  object State extends StateFunctions {
    def apply[S, A](f: S => (S, A)): State[S, A] = new StateT[Id, S, A] {
      def apply(s: S) = f(s)
    }
  }

  type StoreT[F[_], A, B] = IndexedStoreT[F, A, A, B]
  type IndexedStore[I, A, B] = IndexedStoreT[Id, I, A, B]
  type Store[A, B] = StoreT[Id, A, B]
  // flipped
  type |-->[A, B] = Store[B, A]
  object StoreT extends StoreTFunctions with StoreTInstances {
    def apply[F[_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
      storeT(r)
  }
  object IndexedStore {
    def apply[I, A, B](f: A => B, i: I): IndexedStore[I, A, B] = IndexedStoreT.indexedStore(i)(f)
  }
  object Store {
    def apply[A, B](f: A => B, a: A): Store[A, B] = StoreT.store(a)(f)
  }


  type ReaderWriterStateT[F[_], -R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A]
  object ReaderWriterStateT extends ReaderWriterStateTFunctions with ReaderWriterStateTInstances {
    def apply[F[_], R, W, S, A](f: (R, S) => F[(W, A, S)]): ReaderWriterStateT[F, R, W, S, A] = IndexedReaderWriterStateT[F, R, W, S, S, A] { (r: R, s: S) => f(r, s) }
  }
  type IndexedReaderWriterState[-R, W, -S1, S2, A] = IndexedReaderWriterStateT[Id, R, W, S1, S2, A]
  object IndexedReaderWriterState extends ReaderWriterStateTFunctions with ReaderWriterStateTInstances {
    def apply[R, W, S1, S2, A](f: (R, S1) => (W, A, S2)): IndexedReaderWriterState[R, W, S1, S2, A] = IndexedReaderWriterStateT[Id, R, W, S1, S2, A] { (r: R, s: S1) => f(r, s) }
  }
  type ReaderWriterState[-R, W, S, A] = ReaderWriterStateT[Id, R, W, S, A]
  object ReaderWriterState extends ReaderWriterStateTFunctions with ReaderWriterStateTInstances {
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

  /**
   * An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
   *
   * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
   */
  type ValidationNel[+E, +X] = Validation[NonEmptyList[E], X]

  type FirstOf[A] = A @@ Tags.FirstVal
  type LastOf[A] = A @@ Tags.LastVal
  type MinOf[A] = A @@ Tags.MinVal
  type MaxOf[A] = A @@ Tags.MaxVal

  type FirstOption[A] = Option[A] @@ Tags.First
  type LastOption[A] = Option[A] @@ Tags.Last
  type MinOption[A] = Option[A] @@ Tags.Min
  type MaxOption[A] = Option[A] @@ Tags.Max

  //
  // Lens type aliases
  //
  /** A lens that doesn't transform the type of the record. */
  type Lens[A, B] = LensFamily[A, A, B, B]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object Lens extends LensFunctions with LensInstances {
    def apply[A, B](r: A => Store[B, A]): Lens[A, B] =
      lens(r)
  }

  type @>[A, B] = Lens[A, B]

  //
  // Partial Lens type aliases
  //
  /** A partial lens that doesn't transform the type of the record. */
  type PLens[A, B] = PLensFamily[A, A, B, B]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object PLens extends PLensFunctions with PLensInstances {
    def apply[A, B](r: A => Option[Store[B, A]]): PLens[A, B] =
      plens(r)
  }

  type @?>[A, B] = PLens[A, B]

  type PIndexedStateT[F[_], -S1, S2, A] = IndexedStateT[F, S1, S2, Option[A]]
  type PStateT[F[_], S, A] = PIndexedStateT[F, S, S, A]

  type PIndexedState[-S1, S2, A] = PIndexedStateT[Id, S1, S2, A]
  type PState[S, A] = PStateT[Id, S, A]

  type IndexedConts[W[_], R, O, A] = IndexedContsT[W, Id, R, O, A]
  object IndexedConts extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[W[_], R, O, A](f: W[A => O] => R): IndexedConts[W, R, O, A] = IndexedContsT[W, Id, R, O, A](f)
  }
  type IndexedContT[M[_], R, O, A] = IndexedContsT[Id, M, R, O, A]
  object IndexedContT extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[M[_], R, O, A](f: (A => M[O]) => M[R]): IndexedContT[M, R, O, A] = IndexedContsT[Id, M, R, O, A](f)
  }
  type IndexedCont[R, O, A] = IndexedContT[Id, R, O, A]
  object IndexedCont extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[W[_], R, O, A](f: (A => O) => R): IndexedCont[R, O, A] = IndexedContsT[Id, Id, R, O, A](f)
  }
  type ContsT[W[_], M[_], R, A] = IndexedContsT[W, M, R, R, A]
  object ContsT extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[W[_], M[_], R, A](f: W[A => M[R]] => M[R]): ContsT[W, M, R, A] = IndexedContsT[W, M, R, R, A](f)
  }
  type Conts[W[_], R, A] = ContsT[W, Id, R, A]
  object Conts extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[W[_], R, A](f: W[A => R] => R): Conts[W, R, A] = IndexedContsT[W, Id, R, R, A](f)
  }
  type ContT[M[_], R, A] = ContsT[Id, M, R, A]
  object ContT extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[M[_], R, A](f: (A => M[R]) => M[R]): ContT[M, R, A] = IndexedContsT[Id, M, R, R, A](f)
  }
  type Cont[R, A] = ContT[Id, R, A]
  object Cont extends IndexedContsTFunctions with IndexedContsTInstances {
    def apply[R, A](f: (A => R) => R): Cont[R, A] = IndexedContsT[Id, Id, R, R, A](f)
  }

  @deprecated("Cojoin has been merged into Cobind", "7.1")
  type Cojoin[F[_]] = Cobind[F]

  @deprecated("Cojoin has been merged into Cobind", "7.1")
  val Cojoin = Cobind
}
