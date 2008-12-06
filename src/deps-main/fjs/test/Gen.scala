package fjs.test

import fj.test.Rand
import fj.F
import fjs.F._

sealed abstract class Gen[+A] {
  val gen: Int => Rand => A

  def apply(i: Int, r: Rand) = gen(i)(r)

  import Gen._
  
  def >[B](f: A => B): Gen[B] = this map f

  def >>=[B](f: A => Gen[B]): Gen[B] = this bind (f andThen ((x: Gen[B]) => (x: fj.test.Gen[B])))

  def apply[B](g: Gen[A => B]): Gen[B] = (this: fj.test.Gen[A]) apply (g map ((f: A => B) => (f: F[A, B])))

  def >>=[B, C](gb: Gen[B], f: A => B => C): Gen[C] =
    this.bind(gb, f andThen (g => g: F[B, C]))

  def >>=[B, C, D](gb: Gen[B], gc: Gen[C], f: A => B => C => D): Gen[D] =
    gc(this >>= (gb, f))

  def >>=[B, C, D, E](gb: Gen[B], gc: Gen[C], gd: Gen[D], f: A => B => C => D => E): Gen[E] =
    gd(this >>= (gb, gc, f))

  def >>=[B, C, D, E, F$](gb: Gen[B], gc: Gen[C], gd: Gen[D], ge: Gen[E], f: A => B => C => D => E => F$): Gen[F$] =
    ge(this >>= (gb, gc, gd, f))

  def >>=[B, C, D, E, F$, G](gb: Gen[B], gc: Gen[C], gd: Gen[D], ge: Gen[E], gf: Gen[F$], f: A => B => C => D => E => F$ => G): Gen[G] =
    gf(this >>= (gb, gc, gd, ge, f))

  def >>=[B, C, D, E, F$, G, H](gb: Gen[B], gc: Gen[C], gd: Gen[D], ge: Gen[E], gf: Gen[F$], gg: Gen[G], f: A => B => C => D => E => F$ => G => H): Gen[H] =
    gg(this >>= (gb, gc, gd, ge, gf, f))

  def >>=[B, C, D, E, F$, G, H, I](gb: Gen[B], gc: Gen[C], gd: Gen[D], ge: Gen[E], gf: Gen[F$], gg: Gen[G], gh: Gen[H], f: A => B => C => D => E => F$ => G => H => I): Gen[I] =
    gh(this >>= (gb, gc, gd, ge, gf, gg, f))

  def filter[AA >: A](p: AA => Boolean): Gen[AA] = (this: fj.test.Gen[AA]) filter (p(_: AA): java.lang.Boolean)

  def map[B](f: A => B): Gen[B] = (this: fj.test.Gen[A]) map f

  def flatMap[B](f: A => Gen[B]): Gen[B] = (this: fj.test.Gen[A]) bind (f(_: A): fj.test.Gen[B])
}

import fj.F

object Gen {
  implicit def SGen_Gen[A](g: Gen[A]): fj.test.Gen[A] =
    fj.test.Gen.gen(g.gen compose ((_: java.lang.Integer).intValue) andThen (x => x: F[Rand, A]))

  implicit def Gen_SGen[A](g: fj.test.Gen[A]): Gen[A] = (i: Int) => (r: Rand) => g.gen(i, r)

  implicit def gen[A](f: Int => Rand => A): Gen[A] = new Gen[A] {
    val gen = f
  }
}
