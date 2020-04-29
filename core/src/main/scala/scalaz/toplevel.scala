package scalaz

import scalaz.Id.Id

// TODO separate each file?

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

object StateT extends StateTInstances with StateTFunctions {
  def apply[S, F[_], A](f: S => F[(S, A)]): StateT[S, F, A] = IndexedStateT[S, S, F, A](f)
  def liftM[F[_]: Monad, S, A](fa: F[A]): StateT[S, F, A] = MonadTrans[StateT[S, ?[_], ?]].liftM(fa)

  def hoist[F[_]: Monad, G[_]: Monad, S, A](nat: F ~> G): StateT[S, F, ?] ~> StateT[S, G, ?] =
    λ[StateT[S, F, ?] ~> StateT[S, G, ?]](st => StateT((s: S) => nat(st.run(s))))

  def get[F[_]: Monad, S]: StateT[S, F, S] = MonadState[StateT[S, F, ?], S].get
  def gets[F[_]: Monad, S, A](f: S => A): StateT[S, F, A] = MonadState[StateT[S, F, ?], S].gets(f)
  def put[F[_]: Monad, S](s: S): StateT[S, F, Unit] = MonadState[StateT[S, F, ?], S].put(s)
  def modify[F[_]: Monad, S](f: S => S): StateT[S, F, Unit] = MonadState[StateT[S, F, ?], S].modify(f)
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

object ReaderWriterStateT extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
  def apply[R, W, S, F[_], A](f: (R, S) => F[(W, A, S)]): ReaderWriterStateT[R, W, S, F, A] = IndexedReaderWriterStateT[R, W, S, S, F, A] { (r: R, s: S) => f(r, s) }
}

object IndexedReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
  def apply[R, W, S1, S2, A](f: (R, S1) => (W, A, S2)): IndexedReaderWriterState[R, W, S1, S2, A] = IndexedReaderWriterStateT[R, W, S1, S2, Id, A] { (r: R, s: S1) => f(r, s) }
}

object ReaderWriterState extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
  def apply[R, W, S, A](f: (R, S) => (W, A, S)): ReaderWriterState[R, W, S, A] = IndexedReaderWriterStateT[R, W, S, S, Id, A] { (r: R, s: S) => f(r, s) }
}

/**
 * @see [[scalaz.Lens]]
 */
object Lens extends LensInstances with LensFunctions {
  def apply[A, B](r: A => Store[B, A]): Lens[A, B] =
    lens(r)
}

object PLens extends PLensInstances with PLensFunctions {
  def apply[A, B](r: A => Option[Store[B, A]]): PLens[A, B] =
    plens(r)
}

object IndexedConts extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[W[_], R, O, A](f: W[A => O] => R): IndexedConts[W, R, O, A] = IndexedContsT[W, R, O, Id, A](f)
}

object IndexedContT extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[M[_], R, O, A](f: (A => M[O]) => M[R]): IndexedContT[R, O, M, A] = IndexedContsT[Id, R, O, M, A](f)
}

object IndexedCont extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[R, O, A](f: (A => O) => R): IndexedCont[R, O, A] = IndexedContsT[Id, R, O, Id, A](f)
}

object ContsT extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[W[_], M[_], R, A](f: W[A => M[R]] => M[R]): ContsT[W, R, M, A] = IndexedContsT[W, R, R, M, A](f)
}

object Conts extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[W[_], R, A](f: W[A => R] => R): Conts[W, R, A] = IndexedContsT[W, R, R, Id, A](f)
}

object ContT extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[M[_], R, A](f: (A => M[R]) => M[R]): ContT[R, M, A] = IndexedContsT[Id, R, R, M, A](f)
}

object Cont extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[R, A](f: (A => R) => R): Cont[R, A] = IndexedContsT[Id, R, R, Id, A](f)
}

object Select {
  def apply[R, A](f: (A => R) => A): Select[R, A] = SelectT[R, Id, A](f)
}
