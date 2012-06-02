package scalaz

import Id._

sealed trait BijectionT[F[+_], G[+_], A, B] { self =>
  def to(a: A): F[B]
  def from(b: B): G[A]

  import BijectionT._
  import std.AllInstances._

  def flip: BijectionT[G, F, B, A] = new BijectionT[G, F, B, A] {
    def to(a: B): G[A] = self.from(a)

    def from(b: A): F[B] = self.to(b)
  }

  def toK: Kleisli[F, A, B] =
    Kleisli(to(_))

  def fromK: Kleisli[G, B, A] =
    Kleisli(from(_))

  def lens[H[+_]](implicit FF: Functor[H], evF: F[B] =:= H[B], evG: G[A] =:= Id[A]): LensT[H, A, B] =
    LensT(a => FF.map(to(a))(x => Costate(from(_), x)))

  def partial[H[+_]](implicit FF: Functor[H], evF: F[B] =:= H[B], evG: G[A] =:= Id[A]): PLensT[H, A, B] =
    lens[H].partial

  /** alias for `partial` */
  def unary_~[H[+_]](implicit FF: Functor[H], evF: F[B] =:= H[B], evG: G[A] =:= Id[A]) : PLensT[H, A, B] =
    partial[H]

  def bimap[C, X[_, _], D](g: Bijection[C, D])(implicit F: Bifunctor[X], evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Bijection[X[A, C], X[B, D]] =
    bijection(
      F.bimap(_)(to(_), g.to(_)): Id[X[B, D]]
    , F.bimap(_)(from(_), g.from(_)): Id[X[A, C]]
    )

  def ***[C, D](g: Bijection[C, D])(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Bijection[(A, C), (B, D)] =
    bimap[C, Tuple2, D](g)

  def ^^^[C, D](g: Bijection[C, D])(implicit evF: F[B] =:= Id[B], evG: G[A] =:= Id[A]): Bijection[Either[A, C], Either[B, D]] =
    bimap[C, Either, D](g)

  def compose[C](g: BijectionT[F, G, C, A])(implicit FM: Bind[F], GM: Bind[G]): BijectionT[F, G, C, B] =
    bijection(
      (toK <=< g.toK) run _
    , (fromK >=> g.fromK) run _
    )

  /** alias for `compose` */
  def <=<[C](that: BijectionT[F, G, C, A])(implicit FM: Bind[F], GM: Bind[G]): BijectionT[F, G, C, B] = compose(that)

  def andThen[C](that: BijectionT[F, G, B, C])(implicit M: Bind[F], GM: Bind[G]): BijectionT[F, G, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: BijectionT[F, G, B, C])(implicit M: Bind[F], GM: Bind[G]): BijectionT[F, G, A, C] = andThen(that)

}
object BijectionT extends BijectionTFunctions with BijectionTInstances

trait BijectionTFunctions {
  def bijection[F[+_], G[+_], A, B](t: A => F[B], f: B => G[A]): BijectionT[F, G, A, B] =
    new BijectionT[F, G, A, B] {
      def to(a: A) = t(a)
      def from(b: B) = f(b)
    }

  import std.AllInstances._

  type <@>[A, B] =
  Bijection[A, B]

  type Bijection[A, B] =
  BijectionT[Id, Id, A, B]

  def liftBijection[F[+_], G[+_], A, B](t: A => B, f: B => A)(implicit PF: Pointed[F], PG: Pointed[G]): BijectionT[F, G, A, B] =
    bijection(a => PF.point(t(a)), a => PG.point(f(a)))

  def bijectionId[F[+_], G[+_], A](implicit PF: Pointed[F], PG: Pointed[G]): BijectionT[F, G, A, A] =
    liftBijection(x => x, x => x)

  def curryB[F[+_]: Pointed, G[+_]: Pointed, A, B, C]: BijectionT[F, G, (A, B) => C, A => B => C] =
    liftBijection(_.curried, Function.uncurried(_))

  def zipB[F[+_], G[+_], X[_], A, B](implicit PF: Pointed[F], PG: Pointed[G], Z: Zip[X], U: Unzip[X]): BijectionT[F, G, (X[A], X[B]), X[(A, B)]] =
    liftBijection(x => Z.zip(x._1, x._2), U.unzip(_))

  def zipListB[F[+_], G[+_], A, B](implicit PF: Pointed[F], PG: Pointed[G]): BijectionT[F, G, (List[A], List[B]), List[(A, B)]] =
    zipB[F, G, List, A, B]

  def zipEndoB[F[+_], G[+_], A, B](implicit PF: Pointed[F], PG: Pointed[G]): BijectionT[F, G, (Endo[A], Endo[B]), Endo[(A, B)]] =
    zipB[F, G, Endo, A, B]

  def zipReaderB[F[+_], G[+_], T, A, B](implicit PF: Pointed[F], PG: Pointed[G]): BijectionT[F, G, (T => A, T => B), T => (A, B)] =
    zipB[F, G, ({type l[a] = (T => a)})#l, A, B]

  def tuple3B[F[+_]: Pointed, G[+_]: Pointed, A, B, C]: BijectionT[F, G, (A, B, C), (A, (B, C))] =
    liftBijection({ case (a, b, c) => (a, (b, c)) }, { case (a, (b, c)) => (a, b, c) })

  def tuple4B[F[+_]: Pointed, G[+_]: Pointed, A, B, C, D]: BijectionT[F, G, (A, B, C, D), (A, (B, (C, D)))] =
    liftBijection({ case (a, b, c, d) => (a, (b, (c, d))) }, { case (a, (b, (c, d))) => (a, b, c, d) })

  def tuple5B[F[+_]: Pointed, G[+_]: Pointed, A, B, C, D, E]: BijectionT[F, G, (A, B, C, D, E), (A, (B, (C, (D, E))))] =
    liftBijection({ case (a, b, c, d, e) => (a, (b, (c, (d, e)))) }, { case (a, (b, (c, (d, e)))) => (a, b, c, d, e) })

  def tuple6B[F[+_]: Pointed, G[+_]: Pointed, A, B, C, D, E, H]: BijectionT[F, G, (A, B, C, D, E, H), (A, (B, (C, (D, (E, H)))))] =
    liftBijection({ case (a, b, c, d, e, h) => (a, (b, (c, (d, (e, h))))) }, { case (a, (b, (c, (d, (e, h))))) => (a, b, c, d, e, h) })

  def tuple7B[F[+_]: Pointed, G[+_]: Pointed, A, B, C, D, E, H, I]: BijectionT[F, G, (A, B, C, D, E, H, I), (A, (B, (C, (D, (E, (H, I))))))] =
    liftBijection({ case (a, b, c, d, e, h, i) => (a, (b, (c, (d, (e, (h, i)))))) }, { case (a, (b, (c, (d, (e, (h, i)))))) => (a, b, c, d, e, h, i) })

}

trait BijectionTInstances {
  implicit def bijectionTCategory[F[+_], G[+_]](implicit F0: Monad[F], G0: Monad[G]) = new BijectionTCategory[F, G] {
    implicit def F: Monad[F] = F0
    implicit def G: Monad[G] = G0
  }
}

private[scalaz] trait BijectionTCategory[F[+_], G[+_]] extends Category[({type λ[α, β] = BijectionT[F, G, α, β]})#λ] {
  implicit def F: Monad[F]
  implicit def G: Monad[G]

  def compose[A, B, C](bc: BijectionT[F, G, B, C], ab: BijectionT[F, G, A, B]): BijectionT[F, G, A, C] = ab >=> bc

  def id[A] = BijectionT.bijectionId
}
