package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoJoin` */
trait CoJoinV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoJoin[F]
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  ////
}

trait ToCoJoinV  {
  implicit def ToCoJoinV[F[_],A](v: F[A])(implicit F0: CoJoin[F]) =
    new CoJoinV[F,A] { def self = v; implicit def F: CoJoin[F] = F0 }
  implicit def ToCoJoinVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: CoJoin[({type f[a] = F[X, a]})#f]) =
    new CoJoinV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: CoJoin[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToCoJoinVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: CoJoin[({type f[a] = F[X, G, a]})#f]) =
    new CoJoinV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: CoJoin[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToCoJoinVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: CoJoin[({type f[a] = F[X, Id, a]})#f]) =
    new CoJoinV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: CoJoin[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait CoJoinSyntax[F[_]]  {
  implicit def ToCoJoinV[A](v: F[A])(implicit F0: CoJoin[F]): CoJoinV[F, A] = new CoJoinV[F,A] { def self = v; implicit def F: CoJoin[F] = F0 }

  ////

  ////
}
