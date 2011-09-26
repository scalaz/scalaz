package scalaz

trait TraverseLike[F[_]] extends Functor[F] { self =>
  ////
  import Id.id
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
    new Traversal[({type f[x]=State[S,x]})#f]

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = 
    traversal[G].run(fa)(f)
  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] = 
    traversalS[S].run(fa)(f)
  def runTraverseS[S,A,B](fa: F[A], s: S)(f: A => State[S,B]): (F[B], S) =
    traverseS(fa)(f)(s)

  // derived functions
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] = 
    traversal[G].run(fga)(a => a)

  def sequenceS[S,A](fga: F[State[S,A]]): State[S,F[A]] = 
    traversalS[S].run(fga)(a => a)

  def map[A,B](fa: F[A])(f: A => B): F[B] = 
    traversal[Id](id).run(fa)(f)

  def foldL[A,B](fa: F[A], z: B)(f: (B,A) => B): B = foldLShape(fa, z)(f)._2

  def foldLShape[A,B](fa: F[A], z: B)(f: (B,A) => B): (F[Unit], B) = 
    runTraverseS(fa, z)(a => State(b => ((), f(b,a))))

  def reverse[A](fa: F[A]): F[A] = { 
    val (shape, as) = foldLShape(fa, scala.List[A]())((t,h) => h :: t)
    runTraverseS(shape, as)(_ => State(e => (e.head, e.tail)))._1
  }

  def toList[A](fa: F[A]): List[A] = foldL(fa, scala.List[A]())((t,h) => h :: t).reverse 
  def toIndexedSeq[A](fa: F[A]): IndexedSeq[A] = foldL(fa, IndexedSeq[A]())(_ :+ _)
  def toSet[A](fa: F[A]): Set[A] = foldL(fa, Set[A]())(_ + _)

  def zipWith[A,B,C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): (F[C], List[B]) = 
    runTraverseS(fa, toList(fb))(a => 
      State(bs => (f(a,bs.headOption), if (bs.isEmpty) bs else bs.tail)))

  def zipWithL[A,B,C](fa: F[A], fb: F[B])(f: (A,Option[B]) => C): F[C] = zipWith(fa, fb)(f)._1
  def zipWithR[A,B,C](fa: F[A], fb: F[B])(f: (Option[A],B) => C): F[C] = zipWith(fb, fa)((b,oa) => f(oa,b))._1

  def zipL[A,B](fa: F[A], fb: F[B]) = zipWithL(fa, fb)((_,_))
  def zipR[A,B](fa: F[A], fb: F[B]) = zipWithR(fa, fb)((_,_))

  // foldLeft, foldRight, mapAccumL, mapAccumR, map, filter?

  ////
  val traverseSyntax = new scalaz.syntax.TraverseSyntax[F] {}
}

////
/**
 *
 */
////
trait Traverse[F[_]] extends TraverseLike[F] {
  self  =>


}

object Traverse {
  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  ////

  ////
}

trait TraverseInstance[F[_]] extends Traverse[F] with FunctorInstance[F]
