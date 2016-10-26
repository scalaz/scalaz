package scalaz
package scalacheck

import org.scalacheck.Cogen

abstract class ScalaCheckBinding1 {

  implicit val CogenInstance: Divisible[Cogen] = new Divisible[Cogen] {
    def contramap[A, B](a: Cogen[A])(f: B => A) =
      a contramap f
    override def conquer[A] =
      Cogen((seed, _) => seed)
    override def divide[A, B, C](fa: Cogen[A], fb: Cogen[B])(f: C => (A, B)) =
      Cogen{ (seed, c) =>
        val (a, b) = f(c)
        fb.perturb(fa.perturb(seed, a), b)
      }
  }

}
