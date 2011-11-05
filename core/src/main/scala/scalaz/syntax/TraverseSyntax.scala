package scalaz
package syntax

/**Wraps a value `self` and provides methods related to `Traverse` */
trait TraverseV[F[_], A] extends SyntaxV[F[A]] {
  implicit def F: Traverse[F]
  ////

  import Leibniz.===
  import Ident.{id}
  import State.State
  import State.state

  final def tmap[B](f: A => B) =
    F.map(self)(f)

  final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]) =
    G.traverse(self)(f)

  final def sequence[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[F[B]] = {
    val fgb: F[G[B]] = ev.subst[F](self)
    F.sequence(fgb)
  }

  final def traverseS[S, B](f: A => State[S, B]) =
    F.traverseS[S, A, B](self)(f)

  final def runTraverseS[S, B](s: S)(f: A => State[S, B]) =
    F.runTraverseS(self, s)(f)

  final def foldMap[B](f: A => B)(implicit B: Monoid[B]): B = F.foldMap(self)(f)
  final def foldMapIdentity[B](implicit B: Monoid[A]): A = F.foldMapIdentity(self)
  final def reverse: F[A] = F.reverse(self)

  final def toList: List[A] = F.toList(self)
  final def toIndexedSeq(fa: F[A]): IndexedSeq[A] = F.toIndexedSeq(self)
  final def toSet(fa: F[A]): Set[A] = F.toSet(self)

  final def zipWith[B, C](fb: F[B])(f: (A, Option[B]) => C): (F[C], List[B]) = F.zipWith(self, fb)(f)
  final def zipWithL[B, C](fb: F[B])(f: (A, Option[B]) => C): F[C] = F.zipWithL(self, fb)(f)
  final def zipWithR[B, C](fb: F[B])(f: (Option[A], B) => C): F[C] = F.zipWithR(self, fb)(f)
  final def zipL[B](fb: F[B]): F[(A, Option[B])] = F.zipL(self, fb)
  final def zipR[B](fb: F[B]): F[(Option[A], B)] = F.zipR(self, fb)

  ////
}

trait ToTraverseV extends ToFunctorV {
  implicit def ToTraverseV[F[_], A](v: F[A])(implicit F0: Traverse[F]) =
    new TraverseV[F, A] {
      def self = v;
      implicit def F: Traverse[F] = F0
    }
  implicit def ToTraverseVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Traverse[({type f[a] = F[X, a]})#f]) =
    new TraverseV[({type f[a] = F[X, a]})#f, A] {
      def self = v;
      implicit def F: Traverse[({type f[a] = F[X, a]})#f] = F0
    }
  implicit def ToTraverseVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Traverse[({type f[a] = F[X, G, a]})#f]) =
    new TraverseV[({type f[a] = F[X, G, a]})#f, A] {
      def self = v;
      implicit def F: Traverse[({type f[a] = F[X, G, a]})#f] = F0
    }
  implicit def ToTraverseVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Traverse[({type f[a] = F[X, Id, a]})#f]) =
    new TraverseV[({type f[a] = F[X, Id, a]})#f, A] {
      def self = v;
      implicit def F: Traverse[({type f[a] = F[X, Id, a]})#f] = F0
    }

  ////

  ////
}

trait TraverseSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToTraverseV[A](v: F[A])(implicit F0: Traverse[F]): TraverseV[F, A] = new TraverseV[F, A] {
    def self = v;
    implicit def F: Traverse[F] = F0
  }

  ////

  ////
}
