package scalaz

trait Duality[F[_], G[_]] {
  def zap[A, B, C](fa: F[A], gb: G[B])(f: (A, B) => C): C
  def smash[A, B](f: F[A => B], g: G[A]): B = zap(f, g)(_(_))
}

trait DualityInstances {
  val identityDuality: Duality[Id, Id] = new Duality[Id, Id] {
    def zap[A, B, C](a: A, b: B)(f: (A, B) => C): C = f(a, b)
  }
  def productCoproductDuality[F[_], FF[_], G[_], GG[_]](implicit d1: Duality[F, FF], d2: Duality[G, GG]):
    Duality[({ type λ[α] = Either[F[α], G[α]] })#λ, ({ type λ[α] = (FF[α], GG[α]) })#λ] =
      new Duality[({ type λ[α] = Either[F[α], G[α]] })#λ, ({ type λ[α] = (FF[α], GG[α]) })#λ] {
        def zap[A, B, C](a: Either[F[A], G[A]], b: (FF[B], GG[B]))(f: (A, B) => C) =
          a match {
            case Left(fa) => d1.zap(fa, b._1)(f)
            case Right(ga) => d2.zap(ga, b._2)(f)
          }
      }
  def coproductProductDuality[F[_], FF[_], G[_], GG[_]](implicit d1: Duality[FF, F], d2: Duality[GG, G]):
    Duality[({ type λ[α] = (FF[α], GG[α]) })#λ, ({ type λ[α] = Either[F[α], G[α]] })#λ] =
      new Duality[({ type λ[α] = (FF[α], GG[α]) })#λ, ({ type λ[α] = Either[F[α], G[α]] })#λ] {
        def zap[A, B, C](a: (FF[A], GG[A]), b: Either[F[B], G[B]])(f: (A, B) => C) =
          b match {
            case Left(fb) => d1.zap(a._1, fb)(f)
            case Right(gb) => d2.zap(a._2, gb)(f)
          }
      }
}

object Duality extends DualityInstances {
}
