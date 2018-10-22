package scalaz

import scala.language.higherKinds

/**
  * Covariant Day Convolution
  *
  * It is a special form of Functor multiplication.
  * In monoidal category of endofunctors Applicative is a monoid object when Day covolution is used as tensor.
  * If we use Functor composition as tensor then then monoid form a Monad instead of Applicative.
  *
  * Can be seen as generalization of method apply2 from Apply:
  *
  * def apply2(fa => F[A], fb => F[B])(f: (A, B) => C): F[C]
  *
  * trait Day[F[_], G[_], A] { self =>
  *   // ...
  *   val fx: F[X]
  *   val gy: G[Y]
  *   def xya: (X, Y) => A
  * }
  *
  * http://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html
  */
trait Day[F[_], G[_], A] { self =>
  type X
  type Y
  val fx: F[X]
  val gy: G[Y]
  def xya: (X, Y) => A

  def map[B](f: A => B): Day[F, G, B] =
    new Day[F, G, B] {
      type X = self.X
      type Y = self.Y
      val fx: F[X] = self.fx
      val gy: G[Y] = self.gy
      def xya: (X, Y) => B = (x, y) => f(self.xya(x, y))
    }

  /** Swap type constructors order */
  def swapped: Day[G, F, A] = new Day[G, F, A] {
    type X = self.Y
    type Y = self.X
    val fx: G[X] = self.gy
    val gy: F[Y] = self.fx
    def xya: (X, Y) => A = (x, y) => self.xya(y, x)
  }

  /** Apply a natural transformation to the left-hand side of a Day convolution. */
  def trans1[H[_]](nat: F ~> H): Day[H, G, A] = new Day[H, G, A] {
    type X = self.X
    type Y = self.Y
    val fx: H[X] = nat.apply(self.fx)
    val gy: G[Y] = self.gy
    def xya: (X, Y) => A = self.xya
  }

  /** Apply a natural transformation to the right-hand side of a Day convolution. */
  def trans2[H[_]](nat: G ~> H): Day[F, H, A] = new Day[F, H, A] {
    type X = self.X
    type Y = self.Y
    val fx: F[X] = self.fx
    val gy: H[Y] = nat.apply(self.gy)
    def xya: (X, Y) => A = self.xya
  }
}

object Day {
  import Id._

  /** Construct the Day convolution */
  def day[F[_], G[_], A, B](fab: F[A => B], ga: G[A]): Day[F, G, B] = new Day[F, G, B] {
    type X = A => B
    type Y = A
    val fx: F[X] = fab
    val gy: G[Y] = ga
    def xya: (X, Y) => B = (x, y) => x(y)
  }

  def intro1[F[_], A](fa: F[A]): Day[Id, F, A] = new Day[Id, F, A] {
    type X = Unit
    type Y = A
    val fx: Id[X] = ()
    val gy: F[Y] = fa
    def xya: (X, Y) => A = (_, a) => a
  }

  def intro2[F[_], A](fa: F[A]): Day[F, Id, A] = new Day[F, Id, A] {
    type X = A
    type Y = Unit
    val fx: F[X] = fa
    val gy: Id[Y] = ()
    def xya: (X, Y) => A = (a, _) => a
  }

  /** Collapse to second type constructor if first one is Identity */
  def elim1[F[_], A](d: Day[Id, F, A])(implicit FunF: Functor[F]): F[A] = FunF.map(d.gy)(d.xya(d.fx, _))

  /** Collapse to first type constructor if second one is Identity */
  def elim2[F[_], A](d: Day[F, Id, A])(implicit FunF: Functor[F]): F[A] = FunF.map(d.fx)(d.xya(_, d.gy))

  /** Collapse to type constructor if both of them are the same */
  def dap[F[_], A](d: Day[F, F, A])(implicit AF: Applicative[F]): F[A] = AF.apply2(d.fx, d.gy)(d.xya)

  def assoc[F[_], G[_], H[_], A, B](d: Day[F, Day[G, H, ?], A]): Day[Day[F, G, ?], H, A] = {
    new Day[Day[F, G, ?], H, A] {
      type X = (d.X, d.gy.X)
      type Y = d.gy.Y
      val fx: Day[F, G, (d.X, d.gy.X)] = new Day[F, G,(d.X, d.gy.X)] {
        type X = d.X
        type Y = d.gy.X
        val fx: F[X] = d.fx
        val gy: G[Y] = d.gy.fx
        def xya: (X, Y) => (d.X, d.gy.X) = (x, y) => (x, y)
      }
      val gy: H[Y] = d.gy.gy
      def xya: (X, Y) => A = (a, e) => d.xya(a._1, d.gy.xya(a._2, e))
    }
  }

  def disassoc[F[_], G[_], H[_], A](d: Day[Day[F, G, ?], H, A]): Day[F, Day[G, H, ?], A] = new Day[F, Day[G, H, ?], A] {
    type X = d.fx.X
    type Y = (d.fx.Y, d.Y)
    val fx: F[X] = d.fx.fx
    val gy:  Day[G, H, (d.fx.Y, d.Y)] = new Day[G, H, (d.fx.Y, d.Y)] {
      type X = d.fx.Y
      type Y = d.Y
      val fx: G[X] = d.fx.gy
      val gy: H[Y] = d.gy
      def xya: (X, Y) => (d.fx.Y, d.Y) = (x, y) => (x,y)
    }
    def xya: (X, Y) => A = (x: d.fx.X, y: (d.fx.Y, d.Y) ) => d.xya(d.fx.xya(x, y._1), y._2)
  }
}

object DayInstances extends DayInstances1 {

  implicit def comonadDay[F[_], G[_]](implicit CF0: Comonad[F], CG0: Comonad[G]): Comonad[Day[F, G, ?]] = new DayComonad[F, G] {
    def CF: Comonad[F] = CF0
    def CG: Comonad[G] = CG0
  }
}

sealed abstract class DayInstances1 extends DayInstances2 {

  implicit def cobindDay[F[_], G[_]](implicit CF0: Cobind[F], CG0: Cobind[G]): Cobind[Day[F, G, ?]] = new DayCobind[F, G] {
    def CF: Cobind[F] = CF0
    def CG: Cobind[G] = CG0
  }
}

sealed abstract class DayInstances2 extends DayInstances3 {

  implicit def applicativeDay[F[_], G[_]](implicit AF0: Applicative[F], AG0: Applicative[G]): Applicative[Day[F, G, ?]] = new DayApplicative[F, G] {
    def AF: Applicative[F] = AF0
    def AG: Applicative[G] = AG0
  }
}

sealed abstract class DayInstances3 extends DayInstances4 {

  implicit def applyDay[F[_], G[_]](implicit AF0: Apply[F], AG0: Apply[G]): Apply[Day[F, G, ?]] = new DayApply[F, G] {
    def AF: Apply[F] = AF0
    def AG: Apply[G] = AG0
  }
}

sealed abstract class DayInstances4 {

  implicit def functorDay[F[_], G[_]]: Functor[Day[F, G, ?]] = new DayFunctor[F, G] {}
}

private trait DayFunctor[F[_], G[_]] extends Functor[Day[F, G, ?]] {
  override def map[C, D](d: Day[F, G, C])(f: C => D): Day[F, G, D] = d.map(f)
}

private trait DayCobind[F[_], G[_]] extends Cobind[Day[F, G, ?]] with DayFunctor[F, G] {
  def CF: Cobind[F]
  def CG: Cobind[G]

  override def cobind[A, B](fa: Day[F, G, A])(f: Day[F, G, A] => B): Day[F, G, B] = new Day[F, G, B] {
    type X = fa.X
    type Y = fa.Y
    val fx: F[X] = fa.fx
    val gy: G[Y] = fa.gy
    def xya: (X, Y) => B = (_, _) => f(fa)
  }

  override def cojoin[C](wa: Day[F, G, C]): Day[F, G, Day[F, G, C]] = new Day[F, G, Day[F, G, C]] {
    type X = F[wa.X]
    type Y = G[wa.Y]
    val fx: F[X] = CF.cojoin(wa.fx)
    val gy: G[Y] = CG.cojoin(wa.gy)
    def xya: (X, Y) => Day[F, G, C] = (x,y) => {new Day[F, G, C] {
      type X = wa.X
      type Y = wa.Y
      val fx: F[X] = x
      val gy: G[Y] = y
      def xya: (X, Y) => C = (x,y) => wa.xya(x,y)
    }}
  }
}

private trait DayComonad[F[_], G[_]] extends Comonad[Day[F, G, ?]] with DayCobind[F, G] {
  def CF: Comonad[F]
  def CG: Comonad[G]

  def copoint[C](w: Day[F, G, C]): C = w.xya(CF.copoint(w.fx), CG.copoint(w.gy))
}

/** Applicative instance for Day convolution */
private trait DayApply[F[_], G[_]] extends Apply[Day[F, G, ?]] with DayFunctor[F, G] {
  def AF: Apply[F]
  def AG: Apply[G]

  def ap[A,B](d: => Day[F, G, A])(f: => Day[F, G, A => B]): Day[F, G, B] = ap(d, f)

  /* Workaround for: stable identifier required, but d found */
  private def ap[A,B](d: Day[F, G, A], f: Day[F, G, A => B]): Day[F, G, B] = new Day[F, G, B] {
    type X = (f.X, d.X)
    type Y = (f.Y, d.Y)
    val fx: F[X] = AF.ap(d.fx)(AF.map(f.fx)(a => b => (a, b)))
    val gy: G[Y] = AG.ap(d.gy)(AG.map(f.gy)(a => b => (a, b)))
    def xya: (X, Y) => B = (a, b) => f.xya(a._1, b._1)(d.xya(a._2, b._2))
  }
}

private trait DayApplicative[F[_], G[_]] extends Applicative[Day[F, G, ?]] with DayApply[F, G] {
  def AF: Applicative[F]
  def AG: Applicative[G]

  override def point[A](a: => A): Day[F, G, A] = new Day[F, G, A] {
    type X = Unit
    type Y = Unit
    val fx: F[X] = AF.pure(())
    val gy: G[Y] = AG.pure(())
    def xya: (X, Y) => A = (_, _) => a
  }
}