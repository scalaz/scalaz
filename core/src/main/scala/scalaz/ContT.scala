package scalaz

////
/**
 *
 */
////
trait ContT[F[_],R,A] { self =>
  ////
  def apply(k:A => F[R]):F[R]
  def map[B](f:A => B):ContT[F,R,B] = new ContT[F,R,B] {
    def apply(k:B => F[R]):F[R] = self.apply(k compose f)
  }
  def flatMap[B](f:A => ContT[F,R,B]):ContT[F,R,B] = new ContT[F,R,B] {
    def apply(k:B => F[R]):F[R] = self.apply((a:A) => f(a).apply(k))
  }
  ////
}

trait ContTInstances2 {
  implicit def contTFunctor[F[_], R](implicit F0: Functor[F]): Functor[({type λ[α] = ContT[F, R, α]})#λ] = new ContTFunctor[F, R] {
    implicit def F: Functor[F] = F0
  }
}

trait ContTInstances1 extends ContTInstances2 {
  implicit def contTPointed[F[_], R](implicit F0: Pointed[F]): Pointed[({type λ[α] = ContT[F, R, α]})#λ] = new ContTPointed[F, R] {
    implicit def F: Pointed[F] = F0
  }
}

trait ContTInstances0 extends ContTInstances1 {
  implicit def contTMonad[F[_], R](implicit F0: Monad[F]): Monad[({type λ[α] = ContT[F, R, α]})#λ] = new ContTMonad[F, R] {
    implicit def F: Monad[F] = F0
  }
}

trait ContTInstances extends ContTInstances0

trait ContTFunctions extends ContTInstances {
  def runContT[F[_],R,A](cont:ContT[F,R,A])(f:A => F[R]):F[R] = cont(f)
  def constT[F[_],R,A](g: => A):ContT[F,R,A] = new ContT[F,R,A] {
    def apply(k:A => F[R]):F[R] = k(g)
  }
  def contT[F[_],R,A](g:(A => F[R]) => F[R]):ContT[F,R,A] = new ContT[F,R,A] {
    def apply(k:A => F[R]):F[R] = g(k)
  }
  def exitCCT[F[_],R,A](r: => F[R]):ContT[F,R,A] = contT(_ => r)

  def callCCT[F[_],R,A,B](k:(A => ContT[F,R,B]) => ContT[F,R,A]):ContT[F,R,A] =
    contT((c:A=>F[R]) => runContT(k((a:A) => contT((_:B => F[R]) => c(a))))(c))

  implicit def ContTToKleisli[F[_],R,A](f:ContT[F,R,A]):Kleisli[F,A=>F[R],R] = Kleisli[F,A=>F[R],R](k => f(k))
}

object ContT extends ContTFunctions {
  @inline def apply[F[_],R,A](g:(A => F[R]) => F[R]):ContT[F,R,A] = contT(g)
}

private[scalaz] trait ContTFunctor[F[_],R] extends Functor[({type λ[α] = ContT[F, R, α]})#λ] {
  implicit def F: Functor[F]
  override def map[A, B](fa: ContT[F, R, A])(f: A => B): ContT[F, R, B] = fa map f
}

private[scalaz] trait ContTPointed[F[_], R] extends Pointed[({type λ[α] = ContT[F, R, α]})#λ] with ContTFunctor[F, R] {
  implicit def F: Pointed[F]
  def point[A](a: => A): ContT[F, R, A] = ContT.constT(a)
}

private[scalaz] trait ContTMonad[F[_], R] extends Monad[({type λ[α] = ContT[F, R, α]})#λ] with ContTPointed[F, R] {
  implicit def F: Monad[F]
  def bind[A, B](fa: ContT[F, R, A])(f: A => ContT[F, R, B]): ContT[F, R, B] = fa flatMap f
}

// private[scalaz] trait OptionTMonadTrans extends MonadTrans[OptionT] {
//   def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
//     OptionT[G, A](G.map[A, Option[A]](a)((a: A) => Some(a)))

//   def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = OptionT[M, x]})#f ~> ({type f[x] = OptionT[N, x]})#f) {
//     def apply[A](fa: OptionT[M, A]): OptionT[N, A] = OptionT(f.apply(fa.runT))
//   }
// }
