package scalaz

////
/**
 *
 */
////
trait ContT[F[_],R,A] { self =>
  ////
  def apply(k:A => F[R])(implicit F: Functor[F]):F[R]
  def map[B](f:A => B)(implicit F: Functor[F]):ContT[F,R,B] = new ContT[F,R,B] {
    def apply(k:B => F[R])(implicit F: Functor[F]):F[R] = self.apply(k compose f)
  }
  def flatMap[B](f:A => ContT[F,R,B])(implicit F: Monad[F]):ContT[F,R,B] = new ContT[F,R,B] {
    def apply(k:B => F[R])(implicit F: Functor[F]):F[R] = self.apply((a:A) => f(a).apply(k))
  }
  ////
}

trait ContTFunctions {
  def runContT[F[_],R,A](cont:ContT[F,R,A])(f:A => F[R])(implicit F: Functor[F]):F[R] = cont(f)
  def constT[F[_],R,A](g: => A)(implicit F: Functor[F]):ContT[F,R,A] = new ContT[F,R,A] {
    def apply(k:A => F[R])(implicit F: Functor[F]):F[R] = k(g)
  }
  def contT[F[_],R,A](g:(A => F[R]) => F[R])(implicit F: Functor[F]):ContT[F,R,A] = new ContT[F,R,A] {
    def apply(k:A => F[R])(implicit F: Functor[F]):F[R] = g(k)
  }

  def callCCT[F[_],R,A,B](k:(A => ContT[F,R,B]) => ContT[F,R,A])(implicit F: Functor[F]):ContT[F,R,A] =
    contT((c:A=>F[R]) => runContT(k((a:A) => contT((_:B => F[R]) => c(a))))(c))
}

object ContT extends ContTFunctions {
  @inline def apply[F[_],R,A](g:(A => F[R]) => F[R])(implicit F: Functor[F]):ContT[F,R,A] = contT(g)
}

private[scalaz] trait ContTFunctor[F[_],R] extends Functor[({type λ[α] = ContT[F, R, α]})#λ] {
  implicit def F: Functor[F]
  override def map[A, B](fa: ContT[F, R, A])(f: A => B): ContT[F, R, B] = fa map f
}

private[scalaz] trait ContTPointed[F[_], R] extends Pointed[({type λ[α] = ContT[F, R, α]})#λ] with ContTFunctor[F, R] {
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
