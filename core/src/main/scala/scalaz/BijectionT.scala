package scalaz

sealed trait BijectionT[F[_], A, B] {
  def to(a: A): F[B]
  def fr(b: B): A

  import BijectionT._
  import std.AllInstances._

  def toK: Kleisli[F, A, B] =
    Kleisli(to(_))

  def lens(implicit F: Functor[F]): LensT[F, A, B] =
    LensT(a => F.map(to(a))(CostateT((fr(_)): Id[B => A], _)))

  def bimap[C, X[_, _], D](g: Bijection[C, D])(implicit F: Bifunctor[X], ev: F[B] =:= Id[B]): Bijection[X[A, C], X[B, D]] =
    bijection(
      F.bimap(_)(to(_), g.to(_)): Id[X[B, D]]
    , F.bimap(_)(fr(_), g.fr(_))
    )

  def ***[C, D](g: Bijection[C, D])(implicit ev: F[B] =:= Id[B]): Bijection[(A, C), (B, D)] =
    bimap[C, Tuple2, D](g)

  def ^^^[C, D](g: Bijection[C, D])(implicit ev: F[B] =:= Id[B]): Bijection[Either[A, C], Either[B, D]] =
    bimap[C, Either, D](g)

  def compose[C](g: BijectionT[F, C, A])(implicit M: Bind[F]): BijectionT[F, C, B] =
    bijection(
      (toK <=< g.toK) run _
    , x => (g.fr(fr(x)))
    )

  /** alias for `compose` */
  def <=<[C](that: BijectionT[F, C, A])(implicit M: Bind[F]): BijectionT[F, C, B] = compose(that)

  def andThen[C](that: BijectionT[F, B, C])(implicit M: Bind[F]): BijectionT[F, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: BijectionT[F, B, C])(implicit M: Bind[F]): BijectionT[F, A, C] = andThen(that)

}
object BijectionT extends BijectionTFunctions with BijectionTInstances

trait BijectionTFunctions {
  def bijection[F[_], A, B](t: A => F[B], f: B => A): BijectionT[F, A, B] =
    new BijectionT[F, A, B] {
      def to(a: A) = t(a)
      def fr(b: B) = f(b)
    }
  import std.AllInstances._

  type <@>[A, B] =
  Bijection[A, B]

  type Bijection[A, B] =
  BijectionT[Id, A, B]

  def liftBijection[F[_], A, B](t: A => B, f: B => A)(implicit P: Pointed[F]): BijectionT[F, A, B] =
    bijection(a => P.point(t(a)), f)

  def bijectionId[F[_], A](implicit P: Pointed[F]): BijectionT[F, A, A] =
    liftBijection(x => x, x => x)

  def curryB[F[_]: Pointed, A, B, C]: BijectionT[F, (A, B) => C, A => B => C] =
    liftBijection(_.curried, Function.uncurried(_))

  def zipB[F[_], X[_], A, B](implicit P: Pointed[F], Z: Zip[X], U: Unzip[X]): BijectionT[F, (X[A], X[B]), X[(A, B)]] =
    liftBijection(x => Z.zip(x._1, x._2), U.unzip(_))

  def zipListB[F[_], A, B](implicit P: Pointed[F]): BijectionT[F, (List[A], List[B]), List[(A, B)]] =
    zipB[F, List, A, B]

  def zipEndoB[F[_], A, B](implicit P: Pointed[F]): BijectionT[F, (Endo[A], Endo[B]), Endo[(A, B)]] =
    zipB[F, Endo, A, B]

  def zipReaderB[F[_], T, A, B](implicit P: Pointed[F]): BijectionT[F, (T => A, T => B), T => (A, B)] =
    zipB[F, ({type l[a] = (T => a)})#l, A, B]

  def tuple3B[F[_]: Pointed, A, B, C]: BijectionT[F, (A, B, C), (A, (B, C))] =
    liftBijection({ case (a, b, c) => (a, (b, c)) }, { case (a, (b, c)) => (a, b, c) })

  def tuple4B[F[_]: Pointed, A, B, C, D]: BijectionT[F, (A, B, C, D), (A, (B, (C, D)))] =
    liftBijection({ case (a, b, c, d) => (a, (b, (c, d))) }, { case (a, (b, (c, d))) => (a, b, c, d) })

  def tuple5B[F[_]: Pointed, A, B, C, D, E]: BijectionT[F, (A, B, C, D, E), (A, (B, (C, (D, E))))] =
    liftBijection({ case (a, b, c, d, e) => (a, (b, (c, (d, e)))) }, { case (a, (b, (c, (d, e)))) => (a, b, c, d, e) })

  def tuple6B[F[_]: Pointed, A, B, C, D, E, G]: BijectionT[F, (A, B, C, D, E, G), (A, (B, (C, (D, (E, G)))))] =
    liftBijection({ case (a, b, c, d, e, g) => (a, (b, (c, (d, (e, g))))) }, { case (a, (b, (c, (d, (e, g))))) => (a, b, c, d, e, g) })

  def tuple7B[F[_]: Pointed, A, B, C, D, E, G, H]: BijectionT[F, (A, B, C, D, E, G, H), (A, (B, (C, (D, (E, (G, H))))))] =
    liftBijection({ case (a, b, c, d, e, g, h) => (a, (b, (c, (d, (e, (g, h)))))) }, { case (a, (b, (c, (d, (e, (g, h)))))) => (a, b, c, d, e, g, h) })

}

trait BijectionTInstances {
  implicit def bijectionTCategory[F[_]](implicit F0: Monad[F]) = new BijectionTCategory[F] {
    implicit def F: Monad[F] = F0
  }
}

private[scalaz] trait BijectionTCategory[F[_]] extends Category[({type λ[α, β] = BijectionT[F, α, β]})#λ] {
  implicit def F: Monad[F]

  def compose[A, B, C](bc: BijectionT[F, B, C], ab: BijectionT[F, A, B]): BijectionT[F, A, C] = ab >=> bc

  def id[A] = BijectionT.bijectionId
}
