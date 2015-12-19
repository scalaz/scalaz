package scalaz

import Id._

final class BijectionT[F[_], G[_], A, B] private[scalaz](_to: A => F[B], _from: B => G[A]) { self =>
  def to(a: A): F[B] = _to(a)
  def from(b: B): G[A] = _from(b)

  import BijectionT._
  import std.tuple._

  def flip: BijectionT[G, F, B, A] = new BijectionT[G, F, B, A](_from, _to)

  def toK: Kleisli[F, A, B] =
    Kleisli(_to)

  def fromK: Kleisli[G, B, A] =
    Kleisli(_from)

  def lens(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Lens[A, B] =
    Lens(a => Store(from(_), to(a)))

  def partial(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): PLens[A, B] =
    lens.partial

  /** alias for `partial` */
  def unary_~(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]) : PLens[A, B] =
    partial

  def bimap[C, X[_, _], D](g: Bijection[C, D])(implicit F: Bifunctor[X], evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Bijection[X[A, C], X[B, D]] =
    bijection(
      F.bimap(_)(_to andThen evF, g.to(_)): Id[X[B, D]]
    , F.bimap(_)(_from andThen evG, g.from(_)): Id[X[A, C]]
    )

  def ***[C, D](g: Bijection[C, D])(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Bijection[(A, C), (B, D)] =
    bimap[C, Tuple2, D](g)

  def ^^^[C, D](g: Bijection[C, D])(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Bijection[A \/ C, B \/ D] =
    bimap[C, \/, D](g)

  def compose[C](g: BijectionT[F, G, C, A])(implicit FM: Bind[F], GM: Bind[G]): BijectionT[F, G, C, B] =
    bijection(
      (toK <=< g.toK).run
    , (fromK >=> g.fromK).run
    )

  /** alias for `compose` */
  def <=<[C](that: BijectionT[F, G, C, A])(implicit FM: Bind[F], GM: Bind[G]): BijectionT[F, G, C, B] = compose(that)

  def andThen[C](that: BijectionT[F, G, B, C])(implicit M: Bind[F], GM: Bind[G]): BijectionT[F, G, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: BijectionT[F, G, B, C])(implicit M: Bind[F], GM: Bind[G]): BijectionT[F, G, A, C] = andThen(that)

}
object BijectionT extends BijectionTInstances {
  def bijection[F[_], G[_], A, B](t: A => F[B], f: B => G[A]): BijectionT[F, G, A, B] =
    new BijectionT[F, G, A, B](t, f)

  import std.list._, std.function._

  type <@>[A, B] =
  Bijection[A, B]

  type Bijection[A, B] =
  BijectionT[Id, Id, A, B]

  def liftBijection[F[_], G[_], A, B](t: A => B, f: B => A)(implicit PF: Applicative[F], PG: Applicative[G]): BijectionT[F, G, A, B] =
    bijection(a => PF.point(t(a)), a => PG.point(f(a)))

  def bijectionId[F[_], G[_], A](implicit PF: Applicative[F], PG: Applicative[G]): BijectionT[F, G, A, A] =
    liftBijection(x => x, x => x)

  def curryB[A, B, C]: Bijection[(A, B) => C, A => B => C] =
    bijection[Id, Id, (A, B) => C, A => B => C](_.curried, Function.uncurried(_))

  // Left is true, Right is false
  def eitherB[A]: Bijection[A \/ A, (Boolean, A)] =
    bijection[Id, Id, A \/ A, (Boolean, A)](_ match {
      case -\/(a) => (true, a)
      case \/-(a) => (false, a)
    }, {
      case (p, a) => if(p) -\/(a) else \/-(a)
    })

  def zipB[X[_], A, B](implicit Z: Zip[X], U: Unzip[X]): Bijection[(X[A], X[B]), X[(A, B)]] =
    bijection[Id, Id, (X[A], X[B]), X[(A, B)]](x => Z.zip(x._1, x._2), U.unzip(_))

  def zipListB[A, B]: Bijection[(List[A], List[B]), List[(A, B)]] =
    zipB[List, A, B]

  def zipEndoB[A, B]: Bijection[(Endo[A], Endo[B]), Endo[(A, B)]] =
    zipB[Endo, A, B]

  def zipReaderB[T, A, B]: Bijection[(T => A, T => B), T => (A, B)] =
    zipB[T => ?, A, B]

  def tuple3B[A, B, C]: Bijection[(A, B, C), (A, (B, C))] =
    bijection({ case (a, b, c) => (a, (b, c)) }, { case (a, (b, c)) => (a, b, c) })

  def tuple4B[ A, B, C, D]: Bijection[(A, B, C, D), (A, (B, (C, D)))] =
    bijection({ case (a, b, c, d) => (a, (b, (c, d))) }, { case (a, (b, (c, d))) => (a, b, c, d) })

  def tuple5B[ A, B, C, D, E]: Bijection[(A, B, C, D, E), (A, (B, (C, (D, E))))] =
    bijection({ case (a, b, c, d, e) => (a, (b, (c, (d, e)))) }, { case (a, (b, (c, (d, e)))) => (a, b, c, d, e) })

  def tuple6B[ A, B, C, D, E, H]: Bijection[(A, B, C, D, E, H), (A, (B, (C, (D, (E, H)))))] =
    bijection({ case (a, b, c, d, e, h) => (a, (b, (c, (d, (e, h))))) }, { case (a, (b, (c, (d, (e, h))))) => (a, b, c, d, e, h) })

  def tuple7B[ A, B, C, D, E, H, I]: Bijection[(A, B, C, D, E, H, I), (A, (B, (C, (D, (E, (H, I))))))] =
    bijection({ case (a, b, c, d, e, h, i) => (a, (b, (c, (d, (e, (h, i)))))) }, { case (a, (b, (c, (d, (e, (h, i)))))) => (a, b, c, d, e, h, i) })

}


sealed abstract class BijectionTInstances0 {
  implicit def bijectionTSplit[F[_], G[_]](implicit F0: Bind[F], G0: Bind[G]): Split[BijectionT[F, G, ?, ?]] =
    new BijectionTSplit[F, G] {
      implicit def F = F0
      implicit def G = G0
    }
}

sealed abstract class BijectionTInstances extends BijectionTInstances0 {
  implicit def bijectionTCategory[F[_], G[_]](implicit F0: Monad[F], G0: Monad[G]): Category[BijectionT[F, G, ?, ?]] =
    new BijectionTCategory[F, G] {
      implicit def F = F0
      implicit def G = G0
    }
}

private trait BijectionTSplit[F[_], G[_]] extends Split[BijectionT[F, G, ?, ?]] {
  implicit def F: Bind[F]
  implicit def G: Bind[G]

  def compose[A, B, C](bc: BijectionT[F, G, B, C], ab: BijectionT[F, G, A, B]): BijectionT[F, G, A, C] = ab >=> bc

  def split[A, B, C, D](ab: BijectionT[F, G, A, B], cd: BijectionT[F, G, C, D]): BijectionT[F, G, (A, C), (B, D)] =
    BijectionT.bijection(
      { case (a, c) => F.tuple2(ab.to(a), cd.to(c)) },
      { case (b, d) => G.tuple2(ab.from(b), cd.from(d)) }
    )
}

private trait BijectionTCategory[F[_], G[_]] extends Category[BijectionT[F, G, ?, ?]] with BijectionTSplit[F, G] {
  implicit def F: Monad[F]
  implicit def G: Monad[G]

  def id[A] = BijectionT.bijectionId
}

