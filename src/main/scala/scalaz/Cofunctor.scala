package scalaz

trait Cofunctor[F[_]] {
  def comap[A, B](r: F[A], f: B => A): F[B]
}

object Cofunctor {
  implicit def Function1Cofunctor[X]: Cofunctor[PartialApply1Of2[Function1, X]#Flip] = new Cofunctor[PartialApply1Of2[Function1, X]#Flip] {
    def comap[A, B](r: A => X, f: B => A) = r compose f
  }

  implicit val EqualCofunctor = new Cofunctor[Equal] {
    def comap[A, B](r: Equal[A], f: B => A) = Equal.equal[B]((b1, b2) => r equal (f(b1), f(b2)))
  }

  implicit val OrderCofunctor = new Cofunctor[Order] {
    def comap[A, B](r: Order[A], f: B => A) = Order.order[B]((b1, b2) => r order (f(b1), f(b2)))
  }

  implicit val ShowCofunctor = new Cofunctor[Show] {
    def comap[A, B](r: Show[A], f: B => A) = Show.show[B](b => r show (f(b)))
  }
}
