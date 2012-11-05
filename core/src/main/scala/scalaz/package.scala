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
 *  - [[scalaz.Plus]]
 *  - [[scalaz.Each]]
 *  - [[scalaz.Index]]
 *  - [[scalaz.Functor]]
 *  - [[scalaz.Pointed]] extends [[scalaz.Functor]]
 *  - [[scalaz.Contravariant]]
 *  - [[scalaz.Copointed]] extends [[scalaz.Functor]]
 *  - [[scalaz.Apply]] extends [[scalaz.Functor]]
 *  - [[scalaz.Applicative]] extends [[scalaz.Apply]] with [[scalaz.Pointed]]
 *  - [[scalaz.Bind]] extends [[scalaz.Apply]]
 *  - [[scalaz.Monad]] extends [[scalaz.Applicative]] with [[scalaz.Bind]]
 *  - [[scalaz.Cojoin]]
 *  - [[scalaz.Cobind]]
 *  - [[scalaz.Comonad]] extends [[scalaz.Copointed]] with [[scalaz.Cojoin]] with [[scalaz.Cobind]]
 *  - [[scalaz.PlusEmpty]] extends [[scalaz.Plus]]
 *  - [[scalaz.ApplicativePlus]] extends [[scalaz.Applicative]] with [[scalaz.PlusEmpty]]
 *  - [[scalaz.MonadPlus]] extends [[scalaz.Monad]] with [[scalaz.ApplicativePlus]]
 *  - [[scalaz.Foldable]]
 *  - [[scalaz.Traverse]] extends [[scalaz.Functor]] with [[scalaz.Foldable]]
 *
 *  - [[scalaz.Bifunctor]]
 *  - [[scalaz.Bitraverse]] extends [[scalaz.Bifunctor]]
 *  - [[scalaz.ArrId]]
 *  - [[scalaz.Compose]]
 *  - [[scalaz.Category]] extends [[scalaz.ArrId]] with [[scalaz.Compose]]
 *  - [[scalaz.Arrow]] extends [[scalaz.Category]]
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

  implicit val idInstance: Traverse[Id] with Each[Id] with Monad[Id] with Comonad[Id] with Cojoin[Id] with Distributive[Id] with Zip[Id] with Unzip[Id] with Cozip[Id] = Id.id

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

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
  type <~[+F[_], -G[_]] = NaturalTransformation[G, F]
  type ~~>[-F[_,_], +G[_,_]] = BiNaturalTransformation[F, G]

  type ⊥ = Nothing
  type ⊤ = Any

  type |>=|[G[_], F[_]] = MonadPartialOrder[G, F]

  type ReaderT[F[+_], E, +A] = Kleisli[F, E, A]
  type =?>[E, A] = Kleisli[Option, E, A]
  type Reader[E, +A] = ReaderT[Id, E, A]

  type Writer[+W, +A] = WriterT[Id, W, A]
  type Unwriter[+W, +A] = UnwriterT[Id, W, A]

  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = Kleisli[Id, E, A](f)
  }

  object Writer {
    def apply[W, A](w: W, a: A): WriterT[Id, W, A] = WriterT[Id, W, A]((w, a))
  }

  object Unwriter {
    def apply[U, A](u: U, a: A): UnwriterT[Id, U, A] = UnwriterT[Id, U, A]((u, a))
  }

  /** A state transition, representing a function `S => (A, S)`. */
  type State[S, +A] = StateT[Id, S, A]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object State extends StateFunctions {
    def apply[S, A](f: S => (S, A)): State[S, A] = new StateT[Id, S, A] {
      def apply(s: S) = f(s)
    }
  }

  type StoreT[F[+_], A, +B] = IndexedStoreT[F, A, A, B]
  type IndexedStore[+I, -A, +B] = IndexedStoreT[Id, I, A, B]
  type Store[A, +B] = StoreT[Id, A, B]
  // flipped
  type |-->[A, +B] = Store[B, A]
  object StoreT extends StoreTFunctions with StoreTInstances {
    def apply[F[+_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
      storeT(r)
  }
  object IndexedStore {
    def apply[I, A, B](f: A => B, i: I): IndexedStore[I, A, B] = IndexedStoreT.indexedStore(i)(f)
  }
  object Store {
    def apply[A, B](f: A => B, a: A): Store[A, B] = StoreT.store(a)(f)
  }


  type ReaderWriterState[-R, +W, S, +A] = ReaderWriterStateT[Identity, R, W, S, A]

  type RWST[F[+_], -R, +W, S, +A] = ReaderWriterStateT[F, R, W, S, A]

  val RWST: ReaderWriterStateT.type = ReaderWriterStateT

  type RWS[-R, +W, S, +A] = ReaderWriterState[R, W, S, A]

  type Alternative[F[_]] = ApplicativePlus[F]

  /**
   * An [[scalaz.Validation]] with a [[scalaz.NonEmptyList]] as the failure type.
   *
   * Useful for accumulating errors through the corresponding [[scalaz.Applicative]] instance.
   */
  type ValidationNEL[+E, +X] = Validation[NonEmptyList[E], X]

  type FirstOption[A] = Option[A] @@ Tags.First
  type LastOption[A] = Option[A] @@ Tags.Last
  type MinOption[A] = Option[A] @@ Tags.Min
  type MaxOption[A] = Option[A] @@ Tags.Max

  //
  // Lens type aliases
  //
  type LensT[F[+_], A, B] = LensFamilyT[F, A, A, B, B]
  type LensFamily[-A1, +A2, +B1, -B2] = LensFamilyT[Id, A1, A2, B1, B2] 
  type Lens[A, B] = LensT[Id, A, B]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object LensT extends LensTFunctions with LensTInstances {
    def apply[F[+_], A, B](r: A => F[Store[B, A]]): LensT[F, A, B] =
      lensT(r)
  }
  object LensFamily extends LensTFunctions with LensTInstances {
    def apply[A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2]): LensFamily[A1, A2, B1, B2] =
      lensFamily(r)
  }
  object Lens extends LensTFunctions with LensTInstances {
    def apply[A, B](r: A => Store[B, A]): Lens[A, B] =
      lens(r)
  }

  type @>[A, B] = Lens[A, B]

  type PLensT[F[+_], A, B] = PLensFamilyT[F, A, A, B, B]
  type PLensFamily[-A1, +A2, +B1, -B2] = PLensFamilyT[Id, A1, A2, B1, B2]
  type PLens[A, B] = PLensT[Id, A, B]

  // important to define here, rather than at the top-level, to avoid Scala 2.9.2 bug
  object PLens extends PLensTFunctions with PLensTInstances {
    def apply[A, B](r: A => Option[Store[B, A]]): PLens[A, B] =
      plens(r)
  }

  type @?>[A, B] = PLens[A, B]

  type PStateT[F[+_], A, B] = StateT[F, A, Option[B]]

  type PState[A, B] = StateT[Id, A, Option[B]]
}
