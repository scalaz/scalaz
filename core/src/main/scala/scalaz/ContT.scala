package scalaz

////
/**
 *
 */
////
trait ContT[R,F[_],A] { self =>
  ////
  def apply(k:A => F[R])(implicit F: Functor[F]):F[R]
  def map[B](f:A => B)(implicit F: Functor[F]):ContT[R,F,B] = new ContT[R,F,B] {
    def apply(k:B => F[R])(implicit F: Functor[F]):F[R] = self.apply(k compose f)
  }
  def flatMap[B](f:A => ContT[R,F,B])(implicit F: Monad[F]):ContT[R,F,B] = new ContT[R,F,B] {
    def apply(k:B => F[R])(implicit F: Functor[F]):F[R] = self.apply((a:A) => f(a).apply(k))
  }
  ////
}

trait ContTFunctions {
  def runContT[R,F[_],A](cont:ContT[R,F,A])(f:A => F[R])(implicit F: Functor[F]):F[R] = cont(f)
  def constT[R,F[_],A](g: => A)(implicit F: Functor[F]):ContT[R,F,A] = new ContT[R,F,A] {
    def apply(k:A => F[R])(implicit F: Functor[F]):F[R] = k(g)
  }
  def contT[R,F[_],A](g:(A => F[R]) => F[R])(implicit F: Functor[F]):ContT[R,F,A] = new ContT[R,F,A] {
    def apply(k:A => F[R])(implicit F: Functor[F]):F[R] = g(k)
  }

  def callCCT[R,F[_],A,B](k:(A => ContT[R,F,B]) => ContT[R,F,A])(implicit F: Functor[F]):ContT[R,F,A] =
    contT((c:A=>F[R]) => runContT(k((a:A) => contT((_:B => F[R]) => c(a))))(c))
}

object ContT extends ContTFunctions {
  @inline def apply[R,F[_],A](a: => A)(implicit F: Functor[F]):ContT[R,F,A] = constT(a)
}

private[scalaz] trait ContTFunctor[R,F[_]] extends Functor[({type λ[α] = ContT[R, F, α]})#λ] {
  implicit def F: Functor[F]
  override def map[A, B](fa: ContT[R, F, A])(f: A => B): ContT[R, F, B] = fa map f
}

private[scalaz] trait ContTPointed[R, F[_]] extends Pointed[({type λ[α] = ContT[R, F, α]})#λ] with ContTFunctor[R,F] {
  def point[A](a: => A): ContT[R, F, A] = ContT(a)
}

private[scalaz] trait ContTMonad[R, F[_]] extends Monad[({type λ[α] = ContT[R, F, α]})#λ] with ContTPointed[R,F] {
  implicit def F: Monad[F]
  def bind[A, B](fa: ContT[R, F, A])(f: A => ContT[R, F, B]): ContT[R, F, B] = fa flatMap f
}

// private[scalaz] trait OptionTMonadTrans extends MonadTrans[OptionT] {
//   def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
//     OptionT[G, A](G.map[A, Option[A]](a)((a: A) => Some(a)))

//   def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = OptionT[M, x]})#f ~> ({type f[x] = OptionT[N, x]})#f) {
//     def apply[A](fa: OptionT[M, A]): OptionT[N, A] = OptionT(f.apply(fa.runT))
//   }
// }
