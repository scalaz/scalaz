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
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
  /** A [[scalaz.NaturalTransformation]][G, F]. */
  type <~[+F[_], -G[_]] = NaturalTransformation[G, F]
  type ~~>[-F[_,_], +G[_,_]] = BiNaturalTransformation[F, G]

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

  object ReaderT {
    def apply[E, F[_], A](f: E => F[A]): ReaderT[E, F, A] = Kleisli[F, E, A](f)
  }

  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = Kleisli[Id, E, A](f)
  }

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[W, Id, A] = WriterT[W, Id, A]((w, a))
  }

  object Unwriter {
    def apply[U, A](u: U, a: A): UnwriterT[Id, U, A] = UnwriterT[Id, U, A]((u, a))
  }

  /** @template */
  type StateT[S, F[_], A] = IndexedStateT[S, S, F, A]

  /** @template */
  type IndexedState[-S1, S2, A] = IndexedStateT[S1, S2, Id, A]
  
  /** A state transition, representing a function `S => (S, A)`.
    *
    * @template
    */
  type State[S, A] = StateT[S, Id, A]

  object StateT extends StateTInstances with StateTFunctions {
    def apply[S, F[_], A](f: S => F[(S, A)]): StateT[S, F, A] = IndexedStateT[S, S, F, A](f)

    def hoist[F[_]: Monad, G[_]: Monad, S, A](nat: F ~> G): StateT[S, F, ?] ~> StateT[S, G, ?] =
      λ[StateT[S, F, ?] ~> StateT[S, G, ?]](st => StateT((s: S) => nat(st.run(s))))
  }
  object IndexedState extends StateFunctions {
    def apply[S1, S2, A](f: S1 => (S2, A)): IndexedState[S1, S2, A] = IndexedStateT[S1, S2, Id, A](f)
  }
  object State extends StateFunctions {
    def apply[S, A](f: S => (S, A)): State[S, A] = StateT[S, Id, A](f)

    def united[S1, S2, A](s: State[S1, State[S2, A]]): State[(S1, S2), A] =
      State(
        ss => {
          val (s1, s2) = ss
          val (ns1, g) = s(s1)
          val (ns2, a) = g(s2)
          ((ns1, ns2), a)
        }
      )

    def hoist[F[_], G[_], S](nat: F ~> G): λ[α => State[S, F[α]]] ~> λ[α => State[S, G[α]]] =
      NaturalTransformation.liftMap[F, G, State[S, ?]](nat)
  }

  /** @template */
  type StoreT[F[_], A, B] = IndexedStoreT[F, A, A, B]

  /** @template */
  type IndexedStore[I, A, B] = IndexedStoreT[Id, I, A, B]

  /** @template */
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

  /** @template */
  type Traced[A, B] = TracedT[Id, A, B]
  def Traced[A, B](f: A => B): Traced[A, B] = TracedT[Id, A, B](f)

  /** @template */
  type ReaderWriterStateT[-R, W, S, F[_], A] = IndexedReaderWriterStateT[R, W, S, S, F, A]
  object ReaderWriterStateT extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S, F[_], A](f: (R, S) => F[(W, A, S)]): ReaderWriterStateT[R, W, S, F, A] = IndexedReaderWriterStateT[R, W, S, S, F, A] { (r: R, s: S) => f(r, s) }
  }

  /** @template */
  type IndexedReaderWriterState[-R, W, -S1, S2, A] = IndexedReaderWriterStateT[R, W, S1, S2, Id, A]
  object IndexedReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S1, S2, A](f: (R, S1) => (W, A, S2)): IndexedReaderWriterState[R, W, S1, S2, A] = IndexedReaderWriterStateT[R, W, S1, S2, Id, A] { (r: R, s: S1) => f(r, s) }
  }

  /** @template */
  type ReaderWriterState[-R, W, S, A] = ReaderWriterStateT[R, W, S, Id, A]
  object ReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
    def apply[R, W, S, A](f: (R, S) => (W, A, S)): ReaderWriterState[R, W, S, A] = IndexedReaderWriterStateT[R, W, S, S, Id, A] { (r: R, s: S) => f(r, s) }
  }
  type IRWST[-R, W, -S1, S2, F[_], A] = IndexedReaderWriterStateT[R, W, S1, S2, F, A]
  val IRWST: IndexedReaderWriterStateT.type = IndexedReaderWriterStateT
  type IRWS[-R, W, -S1, S2, A] = IndexedReaderWriterState[R, W, S1, S2, A]
  val IRWS: IndexedReaderWriterState.type = IndexedReaderWriterState
  type RWST[-R, W, S, F[_], A] = ReaderWriterStateT[R, W, S, F, A]
  val RWST: ReaderWriterStateT.type = ReaderWriterStateT
  type RWS[-R, W, S, A] = ReaderWriterState[R, W, S, A]
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
  /** A partial lens that doesn't transform the type of the record.
    *
    * @template
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

  /** @template */
  type PIndexedStateT[F[_], -S1, S2, A] = IndexedStateT[S1, S2, F, Option[A]]

  /** @template */
  type PStateT[F[_], S, A] = PIndexedStateT[F, S, S, A]

  /** @template */
  type PIndexedState[-S1, S2, A] = PIndexedStateT[Id, S1, S2, A]

  /** @template */
  type PState[S, A] = PStateT[Id, S, A]

  /** @template */
  type IndexedConts[W[_], R, O, A] = IndexedContsT[W, R, O, Id, A]
  object IndexedConts extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, O, A](f: W[A => O] => R): IndexedConts[W, R, O, A] = IndexedContsT[W, R, O, Id, A](f)
  }

  /** @template */
  type IndexedContT[R, O, M[_], A] = IndexedContsT[Id, R, O, M, A]
  object IndexedContT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[M[_], R, O, A](f: (A => M[O]) => M[R]): IndexedContT[R, O, M, A] = IndexedContsT[Id, R, O, M, A](f)
  }
  
  /** @template */
  type IndexedCont[R, O, A] = IndexedContT[R, O, Id, A]
  object IndexedCont extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[R, O, A](f: (A => O) => R): IndexedCont[R, O, A] = IndexedContsT[Id, R, O, Id, A](f)
  }

  /** @template */
  type ContsT[W[_], R, M[_], A] = IndexedContsT[W, R, R, M, A]
  object ContsT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], M[_], R, A](f: W[A => M[R]] => M[R]): ContsT[W, R, M, A] = IndexedContsT[W, R, R, M, A](f)
  }

  /** @template */
  type Conts[W[_], R, A] = ContsT[W, R, Id, A]
  object Conts extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[W[_], R, A](f: W[A => R] => R): Conts[W, R, A] = IndexedContsT[W, R, R, Id, A](f)
  }

  /** @template */
  type ContT[R, M[_], A] = ContsT[Id, R, M, A]
  object ContT extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[M[_], R, A](f: (A => M[R]) => M[R]): ContT[R, M, A] = IndexedContsT[Id, R, R, M, A](f)
  }

  /** @template */
  type Cont[R, A] = ContT[R, Id, A]
  object Cont extends IndexedContsTInstances with IndexedContsTFunctions {
    def apply[R, A](f: (A => R) => R): Cont[R, A] = IndexedContsT[Id, R, R, Id, A](f)
  }

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

  val Void: VoidModule = VoidImpl
  type Void = Void.Void
}
