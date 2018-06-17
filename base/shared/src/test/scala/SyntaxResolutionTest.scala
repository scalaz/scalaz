package scalaz

import scala.List

import Scalaz._

/* Tests that the various syntax macros work for the typeclasses which use them. */
object SyntaxResolutionTest {

  def _applicative[F[_]: Applicative, A](a: A): F[A] = a.pure[F]

  def _apply[F[_]: Apply, A, B](fa: F[A], f: F[A => B]): F[B] = fa.ap(f)

  def _bifunctor[F[_, _]: Bifunctor, A, B, S, T](fab: F[A, B], as: A => S, bt: B => T) = {
    fab.lmap(as): F[S, B]
    fab.map(bt): F[A, T]
    fab.bimap(as, bt): F[S, T]
  }

  def _bind[F[_]: Bind, A, B](fa: F[A], f: A => F[B]): F[B] = fa.flatMap(f)

  def _choice[F[_, _]: Choice, A, B, C](fab: F[A, B]) = {
    fab.leftchoice[C]: F[A \/ C, B \/ C]
    fab.rightchoice[C]: F[C \/ A, C \/ B]
  }

  def _cobind[F[_]: Cobind, A, B](fa: F[A], f: F[A] => B): F[B] = fa.cobind(f)

  def _comonad[F[_]: Comonad, A](fa: F[A]): A = fa.copoint

  def _compose[F[_, _]: Compose, A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C] = fbc.compose(fab)

  def _foldable[F[_]: Foldable, A, B, M: Monoid](fa: F[A], b: B, m: M) = {
    fa.foldMap((a: A) => m): M
    fa.foldLeft(b)((_, _) => b): B
    fa.foldRight(b)((_, _) => b): B
    fa.toList: List[A]
  }

  def _functor[F[_]: Functor, A, B](fa: F[A], f: A => B): F[B] = fa.map(f)

  def _invariantFunctor[F[_]: InvariantFunctor, A, B](fa: F[A], f: A => B, g: B => A): F[B] = fa.imap(f)(g)

  def _phantom[F[_]: Phantom, A, B](fa: F[A]): F[B] = fa.pmap

  def _profunctor[F[_, _]: Profunctor, A, B, C, D](fab: F[A, B], ca: C => A, bd: B => D) = {
    fab.lmap(ca): F[C, B]
    fab.map(bd): F[A, D]
    fab.dimap(ca)(bd): F[C, D]
  }

  def _semigroup[A: Semigroup](a1: A, a2: A): A = a1.mappend(a2)

  def _debug[A: Debug](a: A) = {
    a.debugs: String
    a.debug: data.Cord
  }

  def _eq[A: Eq](a: A): Boolean = a === a

  def _strong[F[_, _]: Strong, A, B, C](fab: F[A, B]) = {
    fab.first[C]: F[(A, C), (B, C)]
    fab.second[C]: F[(C, A), (C, B)]
  }

  def _traversable[F[_]: Traversable, G[_]: Applicative, A, B](fa: F[A], f: A => G[B]): G[F[B]] = fa.traverse(f)

  def _void(v: Void): Unit = {
    v.absurd[Int]
    v.absurd[Nothing]
    v.absurd: Int
    v.absurd: Nothing
  }
}
