package scalaz

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  ////
  import Ident.id
  import State.State
  import State.state

  def traverseImpl[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]]

  class Traversal[G[_]](implicit G: Applicative[G]) { 
    def run[A,B](fa: F[A])(f: A => G[B]): G[F[B]] = traverseImpl[G,A,B](fa)(f)
    // def ***
  }

  // reduce - given monoid
  def traversal[G[_]:Applicative]: Traversal[G] = 
    new Traversal[G]
  def traversalS[S]: Traversal[({type f[x]=State[S,x]})#f] = 
    new Traversal[({type f[x]=State[S,x]})#f]()(State.stateMonad)

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = 
    traversal[G].run(fa)(f)
  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] = 
    traversalS[S].run(fa)(f)
  def runTraverseS[S,A,B](fa: F[A], s: S)(f: A => State[S,B]): (F[B], S) =
    traverseS(fa)(f)(s)

  // derived functions
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] = 
    traversal[G].run[G[A], A](fga)(ga => ga)

  def sequenceS[S,A](fga: F[State[S,A]]): State[S,F[A]] = 
    traversalS[S].run(fga)(a => a)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traversal[Id](id).run(fa)(f)

  // TODO can we provide a default impl of foldR in terms of traverseImpl?

  def foldLShape[A,B](fa: F[A], z: B)(f: (B,A) => B): (F[Unit], B) = 
    runTraverseS(fa, z)(a => State(b => ((), f(b,a))))

  override def foldLeft[A,B](fa: F[A], z: B)(f: (B,A) => B): B = foldLShape(fa, z)(f)._2

  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B = foldLShape(fa, F.zero)((b, a) => F.append(b, f(a)))._2

  def reverse[A](fa: F[A]): F[A] = { 
    val (shape, as) = foldLShape(fa, scala.List[A]())((t,h) => h :: t)
    runTraverseS(shape, as)(_ => State(e => (e.head, e.tail)))._1
  }

  def zipWith[A,B,C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): (F[C], List[B]) = 
    runTraverseS(fa, toList(fb))(a =>
      State(bs => (f(a,bs.headOption), if (bs.isEmpty) bs else bs.tail)))

  def zipWithL[A,B,C](fa: F[A], fb: F[B])(f: (A,Option[B]) => C): F[C] = zipWith(fa, fb)(f)._1
  def zipWithR[A,B,C](fa: F[A], fb: F[B])(f: (Option[A],B) => C): F[C] = zipWith(fb, fa)((b,oa) => f(oa,b))._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] = zipWithL(fa, fb)((_,_))
  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] = zipWithR(fa, fb)((_,_))

  // mapAccumL, mapAccumR, map, filter?

  ////
  val traverseSyntax = new scalaz.syntax.TraverseSyntax[F] {}
}

object Traverse {
  @inline def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  ////

  ////
}

