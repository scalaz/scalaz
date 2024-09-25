package scalaz

/**
  * Covariant Day Convolution
  *
  * Based on Edward Kmett implementation in Haskell: [[https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html]]
  *
  * Day convolution is a special form of Functor multiplication.
  * In monoidal category of endofunctors Applicative is a monoid object when Day covolution is used as tensor.
  * If we use Functor composition as tensor then the monoid form a Monad instead of Applicative.
  *
  * Can be seen as generalization of method apply2 from Apply:
  * {{{
  * def apply2(fa => F[A], fb => F[B])(f: (A, B) => C): F[C]
  *
  * trait Day[F[_], G[_], A] { self =>
  *   // ...
  *   val fx: F[X]
  *   val gy: G[Y]
  *   def xya: (X, Y) => A
  * }
  * }}}
  *
  * @see [[https://www.youtube.com/watch?v=lIWCxRBaQG8 Bartosz Milewski talk, derive Day from multiplying Functors and using Coyoneda]]
  * @see [[https://www.youtube.com/watch?v=cB8DapKQz-I Edward Kmett talk, explains Divisible and Decidable in context of contravariant Day]]
  * @see [[https://blog.functorial.com/posts/2016-08-08-Comonad-And-Day-Convolution.html Phil Freeman blog, connections between Day and Comonads, Comonads transformers, optics]]
  * @see [[https://www.reddit.com/r/haskell/comments/4wvae2/functorial_blog_comonads_and_day_convolution/ Discussion on Reddit with mention about usage for stream processing and intuition based on liftA2 from Applicative and Day]]
  */
trait Day[F[_], G[_], A] { self =>
  type X
  type Y
  val fx: F[X]
  val gy: G[Y]
  def xya: (X, Y) => A

  def map[B](f: A => B): Day[F, G, B] = Day[F, G, B, X, Y](fx, gy, (x, y) => f(self.xya(x, y)))

  def cobind[B](f: Day[F, G, A] => B): Day[F, G, B] = Day[F, G, B, X, Y](fx, gy, (_, _) => f(self))

  /** Swap type constructors order */
  def swapped: Day[G, F, A] = Day[G, F, A, Y, X](gy, fx, (x, y) => self.xya(y, x))

  /** Apply a natural transformation to the left-hand side of a Day convolution. */
  def trans1[H[_]](nat: F ~> H): Day[H, G, A] = Day(nat.apply(fx), gy, xya)

  /** Apply a natural transformation to the right-hand side of a Day convolution. */
  def trans2[H[_]](nat: G ~> H): Day[F, H, A] = Day(fx, nat.apply(gy), xya)
}

object Day extends DayInstances {
  import scalaz.Id.Id

  def apply[F[_], G[_], A, XX, YY](fa: F[XX], gb: G[YY], abc: (XX, YY) => A): Day[F, G, A] =
    new Day[F, G, A] {
      type X = XX
      type Y = YY
      val fx: F[X] = fa
      val gy: G[Y] = gb
      def xya: (X, Y) => A = abc
    }

  /** Construct the Day convolution */
  def day[F[_], G[_], A, B](fab: F[A => B], ga: G[A]): Day[F, G, B] = Day[F, G, B, A => B, A](fab, ga, (x, y) => x(y))

  def intro1[F[_], A](fa: F[A]): Day[Id, F, A] = Day[Id, F, A, Unit, A]((), fa, (_, a) => a)

  def intro2[F[_], A](fa: F[A]): Day[F, Id, A] = Day[F, Id, A, A, Unit](fa, (), (a, _) => a)

  /** Collapse to second type constructor if first one is Identity */
  def elim1[F[_], A](d: Day[Id, F, A])(implicit FF: Functor[F]): F[A] = FF.map(d.gy)(d.xya(d.fx, _))

  /** Collapse to first type constructor if second one is Identity */
  def elim2[F[_], A](d: Day[F, Id, A])(implicit FF: Functor[F]): F[A] = FF.map(d.fx)(d.xya(_, d.gy))

  /** Collapse to type constructor if both of them are the same */
  def dap[F[_], A](d: Day[F, F, A])(implicit AF: Applicative[F]): F[A] = AF.apply2(d.fx, d.gy)(d.xya)

  def assoc[F[_], G[_], H[_], A, B](d: Day[F, Day[G, H, *], A]): Day[Day[F, G, *], H, A] = {
    val fx = Day[F, G, (d.X, d.gy.X), d.X, d.gy.X](d.fx, d.gy.fx, (x, y) => (x, y))
    Day[Day[F, G, *], H, A, (d.X, d.gy.X), d.gy.Y](fx, d.gy.gy, (a, e) => d.xya(a._1, d.gy.xya(a._2, e)))
  }

  def disassoc[F[_], G[_], H[_], A](d: Day[Day[F, G, *], H, A]): Day[F, Day[G, H, *], A] = {
    val gyy:  Day[G, H, (d.fx.Y, d.Y)] = Day[G, H, (d.fx.Y, d.Y), d.fx.Y, d.Y](d.fx.gy, d.gy, (x,y) => (x,y))
    Day[F, Day[G, H, *], A, d.fx.X, (d.fx.Y, d.Y)](d.fx.fx, gyy, (x: d.fx.X, y: (d.fx.Y, d.Y)) => d.xya(d.fx.xya(x, y._1), y._2))
  }
}

sealed abstract class DayInstances extends DayInstances1 {

  implicit def cohoistDay[F[_]](implicit F: Comonad[F]): Cohoist[({type l[X[_], Y] = Day[F, X, Y]})#l] =
    new Cohoist[({type l[X[_], Y] = Day[F, X, Y]})#l] {
      override def lower[G[_], A](a: Day[F, G, A])(implicit G: Cobind[G]): G[A] =
        G.map(a.gy)(a.xya(F.copoint(a.fx), _))

      override def cohoist[M[_], N[_]: Comonad](f: M ~> N): Day[F, M, *] ~> Day[F, N, *] =
        new (Day[F, M, *] ~> Day[F, N, *]){
          def apply[A](a: Day[F, M, A]) = a trans2 f
        }
    }

  implicit def comonadDay[F[_], G[_]](implicit CF0: Comonad[F], CG0: Comonad[G]): Comonad[Day[F, G, *]] = new DayComonad[F, G] {
    def CF: Comonad[F] = CF0
    def CG: Comonad[G] = CG0
  }
}

sealed abstract class DayInstances1 extends DayInstances2 {

  implicit def cobindDay[F[_], G[_]]: Cobind[Day[F, G, *]] = new DayCobind[F, G] {}
}

sealed abstract class DayInstances2 extends DayInstances3 {

  implicit def applicativeDay[F[_], G[_]](implicit AF0: Applicative[F], AG0: Applicative[G]): Applicative[Day[F, G, *]] = new DayApplicative[F, G] {
    def AF: Applicative[F] = AF0
    def AG: Applicative[G] = AG0
  }
}

sealed abstract class DayInstances3 extends DayInstances4 {

  implicit def applyDay[F[_], G[_]](implicit AF0: Apply[F], AG0: Apply[G]): Apply[Day[F, G, *]] = new DayApply[F, G] {
    def AF: Apply[F] = AF0
    def AG: Apply[G] = AG0
  }
}

sealed abstract class DayInstances4 {

  implicit def functorDay[F[_], G[_]]: Functor[Day[F, G, *]] = new DayFunctor[F, G] {}
}

private trait DayFunctor[F[_], G[_]] extends Functor[Day[F, G, *]] {
  override def map[C, D](d: Day[F, G, C])(f: C => D): Day[F, G, D] = d.map(f)
}

private trait DayCobind[F[_], G[_]] extends Cobind[Day[F, G, *]] with DayFunctor[F, G] {
  override def cobind[A, B](fa: Day[F, G, A])(f: Day[F, G, A] => B): Day[F, G, B] = fa.cobind(f)
}

private trait DayComonad[F[_], G[_]] extends Comonad[Day[F, G, *]] with DayCobind[F, G] {
  def CF: Comonad[F]
  def CG: Comonad[G]

  def copoint[C](w: Day[F, G, C]): C = w.xya(CF.copoint(w.fx), CG.copoint(w.gy))
}

private trait DayApply[F[_], G[_]] extends Apply[Day[F, G, *]] with DayFunctor[F, G] {
  def AF: Apply[F]
  def AG: Apply[G]

  def ap[A,B](d: => Day[F, G, A])(f: => Day[F, G, A => B]): Day[F, G, B] = strictAp(d)(f)

  /* Workaround for: stable identifier required, but d found */
  private def strictAp[A,B](d: Day[F, G, A])(f: Day[F, G, A => B]): Day[F, G, B] = {
    val fxx: F[(f.X, d.X)] = AF.ap(d.fx)(AF.map(f.fx)(a => b => (a, b)))
    val gyy: G[(f.Y, d.Y)] = AG.ap(d.gy)(AG.map(f.gy)(a => b => (a, b)))
    val abc: ((f.X, d.X), (f.Y, d.Y)) => B = (a, b) => f.xya(a._1, b._1)(d.xya(a._2, b._2))
    Day(fxx, gyy, abc)
  }
}

private trait DayApplicative[F[_], G[_]] extends Applicative[Day[F, G, *]] with DayApply[F, G] {
  def AF: Applicative[F]
  def AG: Applicative[G]

  override def point[A](a: => A): Day[F, G, A] = Day[F, G, A, Unit, Unit](AF.pure(()), AG.pure(()), (_, _) => a)
}
