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
case class UndoT[S, F[_], A](hstate: StateT[History[S], F, A])

//
// Prioritized Implicits for type class instances
//

trait UndoTsLow2 {
  // Prefer Pointed over Functor
  implicit def undoTFunctor[S, F[_]](implicit F0: Functor[F]) = new UndoTFunctor[S, F] {
    implicit def F: Functor[F] = F0
  }
}

trait UndoTsLow1 extends UndoTsLow2 {
  // Prefer Monad over Pointed
  implicit def undoTPointed[S, F[_]](implicit F0: Pointed[F]) = new UndoTPointed[S, F] {
    implicit def F: Pointed[F] = F0
  }
}

trait UndoTs {
  implicit def undoTMonadTrans[S] = new UndoTMonadTrans[S] {}

  implicit def undoTStateTMonadState[S, F[_]](implicit F0: Monad[F]) = new UndoTMonadState[S, F] {
    implicit def F: Monad[F] = F0

    implicit def HMS: HStateTMonadState[S, F] = MonadState[({type f[s, a] = StateT[s, F, a]})#f, History[S]]
  }
}

object UndoT extends UndoTs

//
// Implementation traits for type class instances
//

trait UndoTFunctor[S, F[_]]
  extends Functor[({type G[x] = UndoT[S, F, x]})#G] {

  implicit def F: Functor[F]

  override def map[A, B](fa: UndoT[S, F, A])(f: A => B) = {
    UndoT[S, F, B](
      StateT(hs => F.map(fa.hstate(hs)) {
        case (a, s) => (f(a), s)
      })
    )
  }
}

trait UndoTPointed[S, F[_]]
  extends Pointed[({type G[x] = UndoT[S, F, x]})#G]
  with UndoTFunctor[S, F] {

  implicit def F: Pointed[F]

  def pure[A](a: => A) = UndoT[S, F, A](StateT(s => F.pure(a, s)))
}

trait UndoTMonadState[S, F[_]]
  extends MonadState[({type HS[X, Y] = UndoT[S, F, Y]})#HS, S]
  with UndoTPointed[S, F] {

  implicit def F: Monad[F]

  implicit def HMS: HStateTMonadState[S, F]

  def bind[A, B](fa: UndoT[S, F, A])(f: A => UndoT[S, F, B]) = {
    UndoT[S, F, B](
      StateT[History[S], F, B](hs => F.bind(fa.hstate(hs)) {
        case (a, s) => f(a).hstate(s)
      })
    )
  }

  def init = UndoT[S, F, S](HMS.gets(s => s.current))

  def put(s: S) = UndoT[S, F, Unit](
    HMS.bind(HMS.init) {
      case History(_, us, rs) => HMS.put(History(s, us, rs))
    }
  )
}

trait UndoTMonadTrans[S]
  extends MonadTrans[({type G[x[_], a] = UndoT[S, x, a]})#G] {

  trait UndoTF[S, G[_]] {
    type f[x] = UndoT[S, G, x]
  }

  def hoist[M[_], N[_]](f: M ~> N) = new (UndoTF[S, M]#f ~> UndoTF[S, N]#f) {
    def apply[A](fa: UndoT[S, M, A]): UndoT[S, N, A] = {
      UndoT[S, N, A](StateT(s => f(fa.hstate(s))))
    }
  }

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): UndoT[S, G, A] = {
    UndoT[S, G, A](StateT(s => G.map(ga)(a => (a, s))))
  }
}


trait Undos {
  def undo[S, F[_]](implicit HMS: HStateTMonadState[S, F], F: Monad[F]): UndoT[S, F, Boolean] =
    UndoT(
      HMS.bind(HMS.init) {
        case History(current, undos, redos) =>
          if (undos.isEmpty)
            HMS.pure(false)
          else
            HMS.bind(HMS.put(History[S](current = undos.head, undos = undos.tail, redos = current :: redos)))(
              _ => HMS.pure(true)
            )
      }
    )

  def ⎌[S, F[_]](implicit HMS: HStateTMonadState[S, F], F: Monad[F]): UndoT[S, F, Boolean] = undo

  def redo[S, F[_]](implicit HMS: HStateTMonadState[S, F], F: Monad[F]): UndoT[S, F, Boolean] =
    UndoT(
      HMS.bind(HMS.init) {
        case History(current, undos, redos) =>
          if (redos.isEmpty)
            HMS.pure(false)
          else
            HMS.bind(HMS.put(History(current = redos.head, undos = current :: undos, redos.tail))) {
              _ => HMS.pure(true)
            }
      }
    )

  def hput[S, F[_]](s: S)(implicit HMS: HStateTMonadState[S, F]): UndoT[S, F, Unit] =
    UndoT(
      HMS.bind(HMS.init) {
        case History(current, undos, redos) =>
          HMS.put(History(s, current :: undos, Nil))
      }
    )

  def evalUndoT[S, F[_] : Monad, A](undoT: UndoT[S, F, A], initial: S): F[A] =
    undoT.hstate ! (History(initial))

  def execUndoT[S, F[_], A](undoT: UndoT[S, F, A], initial: S)(implicit F: Monad[F]): F[S] =
    F.map(undoT.hstate !! History(initial))(_.current)
}

object Undo extends Undos
