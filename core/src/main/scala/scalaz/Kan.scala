package scalaz

/** The right Kan extension of `H` along `G` */
trait Ran[G[_], H[_], A] { ran =>
  def apply[B](f: A => G[B]): H[B]

  def map[B](f: A => B): Ran[G, H, B] = new Ran[G, H, B] {
    def apply[C](k: B => G[C]): H[C] = ran(f andThen k)
  }

  def toAdjoint[F[_]](implicit A: Adjunction[F, G]): H[F[A]] =
    apply(a => A.unit(a))
}

object Ran {
  import Id._

  implicit def ranFunctor[G[_], H[_]]: Functor[Ran[G, H, ?]] =
    new Functor[Ran[G, H, ?]] {
      def map[A,B](r: Ran[G, H, A])(f: A => B) = r map f
    }

  /**
   * The universal property of a right Kan extension. The functor `Ran[G,H,_]` and the
   * natural transformation `gran[G,H,_]` are couniversal in the sense that for any
   * functor `K` and a natural transformation `s` from `K[G[_]]` to `H`, a unique
   * natural transformation `toRan` exists from `K` to `Ran[G,H,_]` such that
   * for all `k`, `gran(toRan(k)) = s(k)`.
   */
  def toRan[G[_], H[_], K[_]:Functor, B](k: K[B])(s: λ[α => K[G[α]]] ~> H): Ran[G, H, B] =
    new Ran[G, H, B] {
      def apply[C](f: B => G[C]) = s(Functor[K].map(k)(f))
    }

  /**
   * `toRan` and `fromRan` witness an adjunction from `Compose[G,_,_]` to `Ran[G,_,_]`.
   */
  def fromRan[G[_], H[_], K[_], B](k: K[G[B]])(s: K ~> Ran[G, H, ?]): H[B] =
    s(k)(x => x)

  def adjointToRan[F[_], G[_], A](f: F[A])(implicit A: Adjunction[F, G]): Ran[G, Id, A] =
    new Ran[G, Id, A] {
      def apply[B](a: A => G[B]) = A.rightAdjunct(f)(a)
    }

  def ranToAdjoint[F[_], G[_], A](r: Ran[G, Id, A])(implicit A: Adjunction[F, G]): F[A] =
    r(a => A.unit(a))

  def composedAdjointToRan[F[_], G[_], H[_], A](h: H[F[A]])(
    implicit A: Adjunction[F, G], H: Functor[H]): Ran[G, H, A] =
      new Ran[G, H, A] {
        def apply[B](f: A => G[B]) = H.map(h)(A.rightAdjunct(_)(f))
      }

  /** This is the natural transformation that defines a right Kan extension. */
  def gran[G[_], H[_], A](r: Ran[G, H, G[A]]): H[A] =
    r(a => a)
}

/** The left Kan extension of `H` along `G` */
trait Lan[G[_], H[_], A] { lan =>
  type I
  def v: H[I]
  def f(gi: G[I]): A

  /**
   * The universal property of a left Kan extension. The functor `Lan[G,H,_]` and the
   * natural transformation `glan[G,H,_]` are universal in the sense that for any
   * functor `F` and a natural transformation `s` from `H` to `F[G[_]]`, a unique
   * natural transformation `toLan` exists from `Lan[G,H,_]` to `F` such that
   * for all `h`, `glan(h).toLan = s(h)`.
   */
  def toLan[F[_]:Functor](s: H ~> λ[α => F[G[α]]]): F[A] =
    Functor[F].map(s(v))(f)

  /**
   * If `G` is left adjoint to `F`, there is a natural isomorphism between
   * `Lan[G,H,_]` and `H[F[_]]`
   */
  def toAdjoint[F[_]](implicit H: Functor[H], A: Adjunction[G,F]): H[F[A]] =
    H.map(v)(A.leftAdjunct(_)(f))

  def map[B](g: A => B): Lan[G, H, B] = new Lan[G, H, B] {
    type I = lan.I
    private[this] val vc = Need(lan.v)
    def v = vc.value
    def f(gi: G[I]) = g(lan f gi)
  }

}

object Lan extends LanInstances {
  import Id._

  implicit def lanApplicative[G[_]:Functor, H[_]:Applicative]: Applicative[Lan[G, H, ?]] =
    new Applicative[Lan[G, H, ?]] with LanApply[G, H] {
      def G = implicitly
      def H = implicitly
      def point[A](a: => A) = new Lan[G,H,A] {
        type I = Unit
        val v = Applicative[H].point(())
        def f(gi: G[I]) = a
      }
    }

  /**
   * `fromLan` and `toLan` witness an adjunction from `Lan[G,_,_]` to `Compose[G,_,_]`:
   */
  def fromLan[F[_], G[_], H[_], B](h: H[B])(s: Lan[G, H, ?] ~> F): F[G[B]] =
    s(glan(h))

  /** The natural transformation that defines a left Kan extension */
  def glan[G[_], H[_], A](h: H[A]): Lan[G, H, G[A]] = 
    new Lan[G, H, G[A]] {
      type I = A
      val v = h
      def f(gi: G[I]) = gi
    }

  def adjointToLan[F[_], G[_], A](ga: G[A])(implicit A: Adjunction[F, G]): Lan[F, Id, A] =
    new Lan[F, Id, A] {
      type I = G[A]
      val v = ga
      def f(gi: F[I]) = A.counit(gi)
    }

  def lanToAdjoint[F[_], G[_], A](lan: Lan[F,Id,A])(implicit A: Adjunction[F, G]): G[A] =
    A.leftAdjunct(lan.v)(lan.f)

  def composedAdjointToLan[F[_], G[_], H[_], A](h: H[G[A]])(
    implicit A: Adjunction[F, G]): Lan[F, H, A] = new Lan[F, H, A] {
      type I = G[A]
      val v = h
      def f(fi: F[I]) = A.counit(fi)
    }
}

sealed abstract class LanInstances0 {
  implicit def lanFunctor[F[_], G[_]]: Functor[Lan[F, G, ?]] =
    new LanFunctor[F, G] { }
}

sealed abstract class LanInstances extends LanInstances0 {
  implicit def lanApply[F[_]: Functor, G[_]: Apply]: Apply[Lan[F, G, ?]] =
    new LanApply[F, G] {
      def G = implicitly
      def H = implicitly
    }
}

private trait LanFunctor[G[_], H[_]] extends Functor[Lan[G, H, ?]] {
  override final def map[A, B](lan: Lan[G, H, A])(g: A => B) = lan map g
}

private trait LanApply[G[_], H[_]] extends Apply[Lan[G, H, ?]] with LanFunctor[G, H] {
  def G: Functor[G]
  def H: Apply[H]

  private[this] abstract class Internal[A] {
    type T
    def value: A
  }

  def ap[A,B](x: => Lan[G, H, A])(xf: => Lan[G, H, A => B]) = new Lan[G, H, B] {
    val xfp = new Internal[Lan[G, H, A => B]] {
      lazy val value = xf
      type T = value.I
    }
    val xp = new Internal[Lan[G,H,A]] {
      lazy val value = x
      type T = value.I
    }
    type I = (xfp.T, xp.T)
    private[this] val vc = Need(H.tuple2(xfp.value.v, xp.value.v))
    def v = vc.value
    def f(gi: G[I]) = xfp.value.f(G.map(gi)(_._1))(xp.value.f(G.map(gi)(_._2)))
  }
}
