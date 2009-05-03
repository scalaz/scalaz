package scalaz

import S._

/**
 * A continuation monad with two answer types.
 */
sealed trait CC[A, B, C] {
  import CC._
  def apply(f: C => A): B
  def +>>[D, E](vm2: CC[D, A, E]) = CCMonadish.gbind(this, ((c: C) => vm2))
  def >==[D, E](f: C => CC[D, A, E]) = CCMonadish.gbind(this, f)
  def ^[D](e2: CC[D, A, C])(implicit s: Semigroup[C]): CC[D, B, C]  =
    CCMonadish.gbind(this, (x: C) => CCMonadish.gbind(e2, (y: C) => CCMonadish.gpure[C, D](x |+| y)))
}

object CC {
  def cc[A, B, C](c: (C => A) => B) = new CC[A, B, C] {
    def apply(f: C => A) = c(f)
  }

  import MA._

  def shift[A, B, C, D, E](f: (C => CC[E, E, A]) => CC[D, B, D]): CC[A, B, C] =
    cc((k: C => A) => f((c: C) => ret[A, E](k(c))).apply(identity(_)))

  def reset[A, B, C](c: CC[B, C, B]): CC[A, A, C] = CC.cc((k: C => A) => k(c.apply(identity(_))))

  def run[A, B](c: CC[A, B, A]): B = c.apply(identity(_))

  trait Monadish[M[_, _, _]] {
    def gpure[A, B](a: A): M[B, B, A]

    def gbind[A, B, C, D, E](s: M[B, C, D], f: D => M[A, B, E]): M[A, C, E]
  }

  trait MW[M[_], P, Q, A] {
    val unMW: M[A]
  }

  def mw[M[_], P, Q, A](ma: M[A]) = new MW[M, P, Q, A] {
    val unMW = ma
  }

  trait MWM[M[_]] {
    type T[A, B, C] = MW[M, A, B, C]
  }

  def MWMonadish[M[_]](implicit m: Monad[M]): Monadish[MWM[M]#T] =
    new Monadish[MWM[M]#T] {
      def gpure[A, B](a: A) = mw[M, B, B, A](m.pure(a))

      def gbind[A, B, C, D, E](x: MWM[M]#T[B, C, D], f: D => MWM[M]#T[A, B, E]) =
        mw[M, A, C, E](m.bind(x.unMW, ((d: D) => f(d).unMW)))
    }

  implicit def CCMonadish = new Monadish[CC] {
    def gpure[A, B](a: A) = cc((k: A => B) => k(a))

    def gbind[A, B, C, D, E](f: CC[B, C, D], h: D => CC[A, B, E]) = cc((k: E => A) => f((s: D) => h(s).apply(k)))
  }

  def ret[A, B](a :A) = CCMonadish.gpure[A, B](a)

}
