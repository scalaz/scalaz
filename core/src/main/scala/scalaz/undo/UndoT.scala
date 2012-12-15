package scalaz
package undo

/**
 * UndoT Monad Transformer.
 *
 * Ported from [[http://hackage.haskell.org/packages/archive/Hedi/0.1.1/doc/html/Undo.html]]
 *
 * Supports undo, redo and hput to push the last state on history. The redo stack is blanked on hput.
 *
 * @author Gerolf Seitz
 * @author Jason Zaugg
 */
final case class UndoT[F[+_], S, +A](hstate: StateT[F, History[S], A]) {
  def apply(initial: S)(implicit F: Functor[F]): F[(History[S], A)] = hstate(History(initial))

  def eval(initial: S)(implicit F: Functor[F]): F[A] = F.map(apply(initial))(_._2)

  def exec(initial: S)(implicit F: Functor[F]): F[S] = F.map(apply(initial))(_._1.current)
  
  def map[B](f: A => B)(implicit F: Functor[F]) = UndoT.mkUndoT[F, S, B](
    hs => F.map(hstate(hs)) {
      case (s, a) => (s, f(a))
    }
  )

  def flatMap[B](f: A => UndoT[F, S, B])(implicit F: Bind[F]): UndoT[F, S, B] = UndoT.mkUndoT[F, S, B](
    hs => F.bind(hstate(hs)) {
      case (s, a) => f(a).hstate(s)
    }
  )
}

//
// Prioritized Implicits for type class instances
//

trait UndoTInstances1 {
  // Prefer Pointed over Functor
  implicit def undoTFunctor[S, F[+_]](implicit F0: Functor[F]): Functor[({type G[+x] = UndoT[F, S, x]})#G] = new UndoTFunctor[S, F] {
    implicit def F: Functor[F] = F0
  }
}

trait UndoTInstances0 extends UndoTInstances1 {
  // Prefer Monad over Pointed
  implicit def undoTPointed[S, F[+_]](implicit F0: Pointed[F]): Pointed[({type G[+x] = UndoT[F, S, x]})#G] = new UndoTPointed[S, F] {
    implicit def F: Pointed[F] = F0
  }
}

trait UndoTInstances extends UndoTInstances0 {
  implicit def undoTMonadTrans[S]: Hoist[({type G[x[+_], +a] = UndoT[x, S, a]})#G] = new UndoTHoist[S] {}

  implicit def undoTMonadState[S, F[+_]](implicit F0: Monad[F]): MonadState[({type HS[X, Y] = UndoT[F, X, Y]})#HS, S] = new UndoTMonadState[S, F] {
    implicit def F: Monad[F] = F0

    implicit def HMS: HStateTMonadState[F, S] = MonadState[({type f[s, +a] = StateT[F, s, a]})#f, History[S]]
  }
}

trait UndoTFunctions {
  def mkUndoT[F[+_], S, A](f: History[S] => F[(History[S], A)]): UndoT[F, S, A] =
    new UndoT[F, S, A](StateT(f))

  def bindInit[F[+_], S, B](f: History[S] => StateTHistory[F, S, B])(implicit HMS: HStateTMonadState[F, S]): UndoT[F, S, B] = {
    import HMS._
    UndoT(bind(init)(f))
  }

  /**
   * Restores the latest item in the history
   *
   */
  def undo[F[+_], S](implicit HMS: HStateTMonadState[F, S]): UndoT[F, S, Boolean] = {
    import HMS._
    bindInit {
      case History(current, Nil, redos) =>
        point(false)
      case History(current, u :: us, redos) =>
        val newHistory = History[S](current = u, undos = us, redos = current :: redos)
        bind(put(newHistory))(_ => point(true))
    }
  }

  /**Reverses the previous undo */
  def redo[F[+_], S](implicit HMS: HStateTMonadState[F, S]): UndoT[F, S, Boolean] = {
    import HMS._
    bindInit {
      case History(current, undos, Nil) =>
        point(false)
      case History(current, undos, r :: rs) =>
        val newHistory = History(current = r, undos = current :: undos, rs)
        bind(put(newHistory))(_ => point(true))
    }
  }

  /**Replace the current state with `s`. The redo stack is cleared. A subsequent `undo` will restore the previous state. */
  def hput[F[+_], S](s: S)(implicit HMS: HStateTMonadState[F, S]): UndoT[F, S, Unit] = {
    import HMS._
    bindInit {
      case History(current, undos, redos) => put(History(s, current :: undos, Nil))
    }
  }
}

object UndoT extends UndoTInstances with UndoTFunctions

//
// Implementation traits for type class instances
//

private[scalaz] trait UndoTFunctor[S, F[+_]]
  extends Functor[({type G[+x] = UndoT[F, S, x]})#G] {

  implicit def F: Functor[F]

  override def map[A, B](fa: UndoT[F, S, A])(f: A => B) = fa.map(f)
}

private[scalaz] trait UndoTPointed[S, F[+_]]
  extends Pointed[({type G[x] = UndoT[F, S, x]})#G]
  with UndoTFunctor[S, F] {

  implicit def F: Pointed[F]

  def point[A](a: => A) = UndoT[F, S, A](StateT(s => F.point(s, a)))
}

private[scalaz] trait UndoTMonadState[S, F[+_]]
  extends MonadState[({type HS[X, Y] = UndoT[F, X, Y]})#HS, S]
  with UndoTPointed[S, F] {

  implicit def F: Monad[F]

  implicit def HMS: HStateTMonadState[F, S]

  def bind[A, B](fa: UndoT[F, S, A])(f: A => UndoT[F, S, B]) = fa.flatMap(f)

  def init = UndoT[F, S, S](HMS.gets(s => s.current))

  def put(s: S) = UndoT[F, S, Unit](
    HMS.bind(HMS.init) {
      case History(_, us, rs) => HMS.put(History(s, us, rs))
    }
  )
}

private[scalaz] trait UndoTHoist[S] extends Hoist[({type G[x[+_], a] = UndoT[x, S, a]})#G] {

  trait UndoTF[G[+_], S] {
    type λ[α] = UndoT[G, S, α]
  }

  def hoist[M[+_]: Monad, N[+_]](f: M ~> N) = new (UndoTF[M, S]#λ ~> UndoTF[N, S]#λ) {
    def apply[A](fa: UndoT[M, S, A]): UndoT[N, S, A] = {
      UndoT[N, S,A](StateT(s => f(fa.hstate(s))))
    }
  }

  def liftM[G[+_], A](ga: G[A])(implicit G: Monad[G]): UndoT[G, S, A] = {
    UndoT[G, S, A](StateT(s => G.map(ga)(a => (s, a))))
  }

  implicit def apply[G[+_]: Monad]: Monad[UndoTF[G, S]#λ] = UndoT.undoTMonadState[S, G]
}
