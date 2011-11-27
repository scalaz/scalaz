package scalaz

trait CoKleisli[F[_], A, B] { self =>
  def run(fa: F[A]): B

  def contramapValue[C](f: F[C] => F[A]): CoKleisli[F, C,  B] = new CoKleisli[F, C, B] {
    def run(fc: F[C]): B = self.run(f(fc))
  }

  def map[C](f: B => C): CoKleisli[F, A, C] = new CoKleisli[F, A, C] {
    def run(fa: F[A]) = f(self.run(fa))
  }

  def flatMap[C](f: B => CoKleisli[F, A, C]): CoKleisli[F, A, C] = new CoKleisli[F, A, C] {
    def run(fa: F[A]) = f(self.run(fa)).run(fa)
  }

//  def redaer(implicit i: Identity[A] =:= W[A]): A => B =
//    a => run(id(a))

  def <<=(a: F[A])(implicit F: Functor[F], FC: CoJoin[F]): F[B] =
    F.map(FC.cojoin(a))(run)

  def =>=[C](c: CoKleisli[F, B, C])(implicit F: Functor[F], FC: CoJoin[F]): CoKleisli[F, A, C] =
    CoKleisli(fa => c run (<<=(fa)))

  def compose[C](c: CoKleisli[F, C, A])(implicit F: Functor[F], FC: CoJoin[F]): CoKleisli[F, C, B] =
    c =>= this

  def =<=[C](c: CoKleisli[F, C, A])(implicit F: Functor[F], FC: CoJoin[F]): CoKleisli[F, C, B] =
    compose(c)
}

object CoKleisli extends CoKleisliFunctions with CoKleisliInstances {
  def apply[F[_], A, B](f: F[A] => B): CoKleisli[F, A, B] = new CoKleisli[F, A, B] {
    def run(fa: F[A]): B = f(fa)
  }
}

trait CoKleisliInstances1 {
  implicit def cokleisliArr[F[_]](implicit F0: CoPointed[F]) = new CoKleisliArr[F] {
    override implicit def F = F0
  }
  implicit def cokleisliFirst[F[_]](implicit F0: CoPointed[F]) = new CoKleisliFirst[F] {
    override implicit def F = F0
  }
  implicit def cokleisliArrId[F[_]](implicit F0: CoPointed[F]) = new CoKleisliArrId[F] {
    override implicit def F = F0
  }
}

trait CoKleisliInstances0 extends CoKleisliInstances1 {
  implicit def cokleisliCompose[F[_]](implicit F0: CoJoin[F] with Functor[F]) = new CoKleisliCompose[F] {
    override implicit def F = F0
  }
}

trait CoKleisliInstances extends CoKleisliInstances0 {
  implicit def cokleisliMonad[F[_], R] = new CoKleisliMonad[F, R] {}
  
  implicit def cokleisliArrow[F[_]](implicit F0: CoMonad[F]) = new CoKleisliArrow[F] {
    override implicit def F = F0
  }
}

trait CoKleisliFunctions {
  // TODO
//  type RedaerT[A, F[_], B] = Cokleisli[F, A, B]
//  type Redaer[A, B] = Cokleisli[Need, A, B]

//  def redaer[A, B](r: A => B): Redaer[A, B] =
//    Cokleisli[A, Identity, B](a => r(a.value))
//
//  def ksa[F[_] : CoPointed, A]: Cokleisli[A, F, A] =
//    Cokleisli(a => implicitly[CoPointed[F]].coPoint(a))
}

trait CoKleisliMonad[F[_], R] extends Monad[({type λ[α] = CoKleisli[F, R, α]})#λ] {
  override def ap[A, B](fa: => CoKleisli[F, R, A])(f: => CoKleisli[F, R, (A) => B]) = f flatMap (fa map _)
  def point[A](a: => A) = CoKleisli(_ => a)
  def bind[A, B](fa: CoKleisli[F, R, A])(f: (A) => CoKleisli[F, R, B]) = fa flatMap f
}

trait CoKleisliArr[F[_]] extends Arr[({type λ[α, β] = CoKleisli[F, α, β]})#λ] {
  implicit def F: CoPointed[F]
  def arr[A, B](f: (A) => B) = CoKleisli(a => f(F.copoint(a)))
}

trait CoKleisliFirst[F[_]] extends First[({type λ[α, β] = CoKleisli[F, α, β]})#λ] {
  implicit def F: CoPointed[F]

  def first[A, B, C](f: CoKleisli[F, A, B]) =
    CoKleisli[F, (A, C), (B, C)](w => (f.run(F.map(w)(ac => ac._1)), F.copoint(w)._2))
}

trait CoKleisliArrId[F[_]] extends ArrId[({type λ[α, β] = CoKleisli[F, α, β]})#λ] {
  implicit def F: CoPointed[F]

  override def id[A] = CoKleisli(F.copoint)
}

trait CoKleisliCompose[F[_]] extends Compose[({type λ[α, β] = CoKleisli[F, α, β]})#λ] {
  implicit def F: CoJoin[F] with Functor[F]

  override def compose[A, B, C](f: CoKleisli[F, B, C], g: CoKleisli[F, A, B]) = f compose g
}


trait CoKleisliArrow[F[_]]
  extends Arrow[({type λ[α, β] = CoKleisli[F, α, β]})#λ]
  with CoKleisliArr[F]
  with CoKleisliArrId[F]
  with CoKleisliCompose[F]
  with CoKleisliFirst[F] {
  
  implicit def F: CoMonad[F]
}
