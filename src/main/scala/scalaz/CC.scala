package scalaz

import S._

/**
 * A continuation monad with two answer types.
 */
sealed trait CC[A, B, C] {
  def apply(f: C => A): B
}

object CC {
  def cc[A, B, C](c: (C => A) => B) = new CC[A, B, C] {
    def apply(f: C => A) = c(f)
  }

  import MA._

  def gpure[A, B](a: A) = cc((k: A => B) => k(a))  

  def shift[A, B, C, D, E](f: (C => CC[E, E, A]) => CC[D, B, D]): CC[A, B, C] =
    cc((k: C => A) => f((c: C) => gpure[A, E](k(c))).apply(identity(_)))

  def reset[A, B, C](c: CC[B, C, B]): CC[A, A, C] = CC.cc((k: C => A) => k(c.apply(identity(_))))

  def run[A, B](c: CC[A, B, A]): B = c.apply(identity(_))
}