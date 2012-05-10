package scalaz

import Id._

/**
 * @see [[scalaz.Lens]]
 */
sealed trait CostateT[F[+_], A, +B] {
  def run: (F[A => B], A)

  import CostateT._
  import BijectionT._

  def xmap[X](f: A => X, g: X => A)(implicit F: Functor[F]): CostateT[F, X, B] =
    costateT(F.map(set)(_ compose g), f(pos))

  def bmap[X](b: Bijection[A, X])(implicit F: Functor[F]): CostateT[F, X, B] =
    xmap(b to _, b from _)

  def put(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run._1)(_(a))

  def puts(f: A => A)(implicit F: Functor[F]): F[B] =
    put(f(pos))

  def set: F[A => B] =
    run._1

  def pos: A =
    run._2

  def copoint(implicit F: Copointed[F]): B =
    F.copoint(run._1)(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): CostateT[F, A, C] =
    costateT(mapRunT(k => f compose k))

  def duplicate(implicit F: Comonad[F]): CostateT[F, A, CostateT[F, A, B]] =
    costateT((F.cobind(run._1)(ff => (a: A) => costateT[F, A, B]((ff, a))), pos))

  def cobind[C](f: CostateT[F, A, B] => C)(implicit c: Cobind[F]): CostateT[F, A, C] =
    costateT((Cobind[F].cobind(run._1)(ff => (a: A) => f(costateT[F, A, B]((ff, a)))), pos))

  /** Two disjoint lenses can be paired */
  def product[C, D](that: CostateT[F, C, D])(implicit M: Bind[F]): CostateT[F, (A, C), (B, D)] =
    CostateT(M.bind(set) { s => M.map(that.set)(t => { (ac: (A, C)) => (s(ac._1), t(ac._2))})}, (pos, that.pos))

  /** alias for `product` */
  def ***[C, D](that: CostateT[F, C, D])(implicit M: Bind[F]): CostateT[F, (A, C), (B, D)] = product(that)

  private def mapRunT[C](f: (A => B) => C)(implicit F: Functor[F]): (F[C], A) =
    (F.map(run._1)(f), run._2)
}

object CostateT extends CostateTFunctions with CostateTInstances {
  def apply[F[+_], A, B](r: (F[A => B], A)): CostateT[F, A, B] =
    costateT(r)
}

object Costate {
  def apply[A, B](f: A => B, a: A): Costate[A, B] = CostateT.costate(a)(f)
}

trait CostateTFunctions {

  def costateT[F[+_], A, B](r: (F[A => B], A)): CostateT[F, A, B] = new CostateT[F, A, B] {
    val run = r
  }

  def costate[A, B](a: A)(f: A => B): Costate[A, B] =
    costateT[Id, A, B](f -> a)
}

trait CostateTInstances2 {
  implicit def costateTFunctor[F[+_], A](implicit F0: Functor[F]) = new CostateTFunctor[F, A] {
    implicit def F: Functor[F] = F0
  }
}
trait CostateTInstances1 extends CostateTInstances2 {
  implicit def costateTCopointed[F[+_], A](implicit F0: Copointed[F]) = new CostateTCopointed[F, A] {
    implicit def F: Copointed[F] = F0
  }
}
trait CostateTInstances0 extends CostateTInstances1 {
  implicit def costateTCobind[F[+_], A](implicit F0: Cobind[F]) = new CostateTCobind[F, A] {
    implicit def F: Cobind[F] = F0
  }
}

trait CostateTInstances extends CostateTInstances0 {
  implicit def costateTComonad[F[+_], A](implicit F0: Comonad[F]) = new CostateTComonad[F, A] {
    implicit def F: Comonad[F] = F0
  }
}

trait CostateTFunctor[F[+_], A0] extends Functor[({type λ[+α]=CostateT[F, A0, α]})#λ]{
  implicit def F: Functor[F]
  override def map[A, B](fa: CostateT[F, A0, A])(f: (A) => B): CostateT[F, A0, B] = fa map f
}

trait CostateTCopointed[F[+_], A0] extends Copointed[({type λ[+α]=CostateT[F, A0, α]})#λ] with CostateTFunctor[F, A0] {
  implicit def F: Copointed[F]
  def copoint[A](p: CostateT[F, A0, A]) = p.copoint
}

trait CostateTCobind[F[+_], A0] extends Cobind[({type λ[+α]=CostateT[F, A0, α]})#λ] {
  implicit def F: Cobind[F]
  def cobind[A, B](fa: CostateT[F, A0, A])(f: (CostateT[F, A0, A]) => B) = fa cobind f
  override def map[A, B](fa: CostateT[F, A0, A])(f: (A) => B): CostateT[F, A0, B] = fa map f
}

trait CostateTComonad[F[+_], A0] extends Comonad[({type λ[+α]=CostateT[F, A0, α]})#λ] with CostateTCobind[F, A0] with CostateTCopointed[F, A0]{
  implicit def F: Comonad[F]
  def cojoin[A](a: CostateT[F, A0, A]) = a.duplicate
}

trait CostateTCohoist[S] extends Cohoist[({type f[g[+_], +a] = CostateT[g, S, a]})#f] {
  def lower[G[+_] : Cobind, A](a: CostateT[G, S, A]) =
    Cobind[G].map(a.run._1)((z: S => A) => z(a.run._2))

  def cohoist[M[+_], N[+_]: Comonad](f: M ~> N) =
    new (({type f[+x] = CostateT[M, S, x]})#f ~> ({type f[+x] = CostateT[N, S, x]})#f) {
      def apply[A](c: CostateT[M, S, A]) = {
        val r = c.run
        CostateT(f(r._1), r._2)
      }
    }
}
