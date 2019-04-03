package scalaz

import scala.language.higherKinds

/**
  * Density Comonad.
  *
  * Density is Left Kan Extension where both Functors are the same.
  *
  * Without any restrictions on F we can define Functor, Cobind, Comonad for Density[F].
  * Density is Comonad for free.
  *
  * @see [[https://hackage.haskell.org/package/kan-extensions/docs/Control-Comonad-Density.html]]
  * @see [[http://comonad.com/reader/2011/a-product-of-an-imperfect-union/]]
  */
trait Density[F[_], Y] { self =>
  type X
  val fb: F[X]
  def f: F[X] => Y

  // Derived methods

  def runDensity: Y = f(fb)

  def map[A](fab: Y => A): Density[F, A] = Density[F,A,X](fb, f andThen fab)

  /**
    * The natural isomorphism between a Comonad F and the Density F.
    *
    * d.lowerDensity andThen liftDensity = d
    */
  def lowerDensity(implicit C: Cobind[F]): F[Y] = C.extend(fb)(f)

  def densityToCoyoneda: Coyoneda[F,X] = Coyoneda[F,X,X](fb)(identity[X])

  /**
    * Density is left Kan extension of a Functor F along itself (Lan f f).
    *
    * lanToDensity(d.densityToLan) == d
    * lanToDensity(l).densityToLan == l
    */
  def densityToLan: Lan[F,F,Y] = new Lan[F, F, Y] {
    type I = X
    def v: F[X] = self.fb
    def f(gi: F[X]): Y = self.f(gi)
  }

  def densityToAdjunction[X[_]](implicit F: Functor[F], A: Adjunction[F,X]): F[X[Y]] =
    F.map(fb)(A.leftAdjunct(_)(f))

  trait DensityLaw {
    def densityIsLeftKan[A,B](kb: F[A], kba: F[A] => B)(implicit F: Equal[B]): Boolean = {
      val d = Density(kb, kba)
      F.equal(Density.lanToDensity(d.densityToLan).runDensity, d.runDensity)
    }

    def leftKanIsDensity[A, B](fa: F[A], f: F[A] => B)(implicit F: Equal[F[A]]): Boolean = {
      val l: Lan[F,F,F[A]] = Lan.glan[F,F,A](fa)
      val l2: Lan[F,F,F[A]] = Density.lanToDensity(l).densityToLan
      F.equal(l2.f(l2.v), l.f(l.v))
    }
  }

  def densityLaw: DensityLaw = new DensityLaw {}
}

object Density extends DensityInstances {
  @inline def apply[F[_], A, B](kb: F[B], kba: F[B] => A): Density[F, A] =
    new Density[F, A] {
      type X = B
      val fb: F[X] = kb
      def f: F[X] => A = kba
    }

  def liftDensity[F[_],Y](fy: F[Y])(implicit W: Comonad[F]): Density[F,Y] =
    Density[F, Y, Y](fy, W.copoint)

  def lanToDensity[F[_], Y](lan: Lan[F,F,Y]): Density[F, Y] =
    Density[F, Y, lan.I](lan.v, lan.f)
}

sealed abstract class DensityInstances extends DensityInstances0 {

  implicit def comonadInstance[F[_]]: Comonad[Density[F, ?]] = new DensityComonad[F] {}
}

sealed abstract class DensityInstances0 {

  /** Density is a free ComonadTrans */
  implicit val comonadTransInstance: ComonadTrans[Density] = new ComonadTrans[Density] {
    def lower[G[_], A](a: Density[G, A])(implicit C: Cobind[G]): G[A] =
      a.lowerDensity
  }
}

/** Density is a free Comonad */
private trait DensityComonad[F[_]] extends Comonad[Density[F, ?]] {

  def map[A, B](fa: Density[F, A])(f: A => B): Density[F, B] = fa.map(f)

  def cobind[A, B](fa: Density[F, A])(ff: Density[F, A] => B): Density[F, B] =
    cojoin(fa).map(ff)

  override def cojoin[A](fa: Density[F, A]): Density[F, Density[F, A]] =
    Density[F, Density[F, A], fa.X](fa.fb, kx => Density[F, A, fa.X](kx, fa.f))

  def copoint[A](p: Density[F, A]): A = p.runDensity
}
