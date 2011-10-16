package scalaz

trait Cokleisli[F[_], A, B] { self =>
  type CokleisliF[A, B] = Cokleisli[F, A, B]
  
  def run(fa: F[A]): B

  def contramapValue[C](f: F[C] => F[A]): CokleisliF[C,  B] = new CokleisliF[C, B] {
    def run(fc: F[C]): B = self.run(f(fc))
  }

  def map[C](f: B => C): CokleisliF[A, C] = new CokleisliF[A, C] {
    def run(fa: F[A]) = f(self.run(fa))
  }

  def flatMap[C](f: B => CokleisliF[A, C]): CokleisliF[A, C] = new CokleisliF[A, C] {
    def run(fa: F[A]) = f(self.run(fa)).run(fa)
  }

//  def redaer(implicit i: Identity[A] =:= W[A]): A => B =
//    a => run(id(a))

  def <<=(a: F[A])(implicit F: Functor[F], FC: Cojoin[F]): F[B] =
    F.map(FC.cojoin(a))(run)

  def =>=[C](c: CokleisliF[B, C])(implicit F: Functor[F], FC: Cojoin[F]): CokleisliF[A, C] =
    Cokleisli(fa => c run (<<=(fa)))

  def =<=[C](c: CokleisliF[C, A])(implicit F: Functor[F], FC: Cojoin[F]): CokleisliF[C, B] =
    c =>= this
}

object Cokleisli extends Cokleislis {
  def apply[A, F[_], B](f: F[A] => B): Cokleisli[F, A, B] = new Cokleisli[F, A, B] {
    def run(fa: F[A]): B = f(fa)
  }


}

trait Cokleislis {
  // TODO
//  type RedaerT[A, F[_], B] = Cokleisli[F, A, B]
//  type Redaer[A, B] = Cokleisli[Need, A, B]

//  def redaer[A, B](r: A => B): Redaer[A, B] =
//    Cokleisli[A, Identity, B](a => r(a.value))
//
//  def ksa[F[_] : CoPointed, A]: Cokleisli[A, F, A] =
//    Cokleisli(a => implicitly[CoPointed[F]].coPoint(a))
}
