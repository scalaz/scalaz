package scalaz

////
////
trait ApplyParent[F[_]] { self: Apply[F] =>
  ////

  /**
   * Repeats an applicative action infinitely
   */
  def forever[A, B](fa: F[A]): F[B] = discardLeft(fa, forever(fa))

  /** Combine `fa` and `fb` according to `Apply[F]` with a function that discards the `A`(s) */
  def discardLeft[A, B](fa: => F[A], fb: => F[B]): F[B] = apply2(fa,fb)((_,b) => b)

  /** Combine `fa` and `fb` according to `Apply[F]` with a function that discards the `B`(s) */
  def discardRight[A, B](fa: => F[A], fb: => F[B]): F[A] = apply2(fa,fb)((a,_) => a)

  /** An `Apply` for `F` in which effects happen in the opposite order. */
  def flip: Apply[F] = new Apply[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      self.map(fa)(f)
    def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
      self.ap(f)(self.map(fa)(a => (f: A => B) => f(a)))
    override def flip: self.type = self
  }

  ////
}
