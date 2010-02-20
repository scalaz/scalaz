package scalaz

trait InvariantFunctor[F[_]] {
  def xmap[A,B](ma: F[A], f: A => B, g: B => A): F[B]
}

object InvariantFunctor {
  
}