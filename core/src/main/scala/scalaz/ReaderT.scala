package scalaz

case class ReaderT[F[_], E, A](runT: E => F[A]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ReaderT[F, E, B] = ReaderT(e => F.map(runT(e))(f))
  def ap[B](f: ReaderT[F, E, A => B])(implicit F: Apply[F]): ReaderT[F, E, B] = ReaderT(e => F.ap(runT(e))(f.runT(e)))
  def flatMap[B](f: A => ReaderT[F, E, B])(implicit F: Monad[F]): ReaderT[F, E, B] = ReaderT(e => F.bind(runT(e))(a => f(a).runT(e)))
}

object ReaderT extends ReaderTInstances with ReaderTFunctions

trait ReaderTInstances3 {
  implicit def readerTFunctor[F[_], E](implicit F0: Functor[F]) = new ReaderTFunctor[F, E] {
    implicit def F = F0
  }
}

trait ReaderTInstances2 extends ReaderTInstances3 {
  implicit def readerTPointed[F[_], E](implicit F0: Pointed[F]) = new ReaderTPointed[F, E ] {
    implicit def F = F0
  }
}

trait ReaderTInstances1 extends ReaderTInstances2 {
  implicit def readerTApply[F[_], E](implicit F0: Apply[F]) = new ReaderTApply[F, E] {
    implicit def F = F0
  }
}

trait ReaderTInstances0 extends ReaderTInstances1 {
  implicit def readerTApplicative[F[_], E](implicit F0: Applicative[F]) = new ReaderTApplicative[F, E] {
    implicit def F = F0
  }
}

trait ReaderTInstances extends ReaderTInstances0 {
  implicit def readerTMonad[F[_], E](implicit F0: Monad[F]) = new ReaderTMonadReader[F, E] {
    implicit def F = F0
  }

  implicit def readerTMonadTrans[E] = new ReaderTMonadTrans[E] {

  }
}

trait ReaderTFunctions

//
// Type class implementation traits
//

trait ReaderTFunctor[F[_], E] extends Functor[({type λ[α]=ReaderT[F, E, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: ReaderT[F, E, A])(f: A => B) = fa map f
}

trait ReaderTPointed[F[_], E] extends Pointed[({type λ[α]=ReaderT[F, E, α]})#λ] with ReaderTFunctor[F, E] {
  implicit def F: Pointed[F]

  def point[A](a: => A) = ReaderT(e => F.point(a))
}

trait ReaderTApply[F[_], E] extends Apply[({type λ[α]=ReaderT[F, E, α]})#λ] with ReaderTFunctor[F, E] {
  implicit def F: Apply[F]

  override def ap[A, B](fa: ReaderT[F, E, A])(f: ReaderT[F, E, A => B]) = fa ap f
}

trait ReaderTApplicative[F[_], E] extends Applicative[({type λ[α]=ReaderT[F, E, α]})#λ] with ReaderTApply[F, E] with ReaderTPointed[F, E] {
  implicit def F: Applicative[F]
}

trait ReaderTMonadReader[F[_], E] extends MonadReader[({type f[s, a] = ReaderT[F, s, a]})#f, E] with ReaderTApplicative[F, E] {
  implicit def F: Monad[F]

  def bind[A, B](fa: ReaderT[F, E, A])(f: (A) => ReaderT[F, E, B]) = fa flatMap f
  def ask = ReaderT[F, E, E](e => F.point(e))
  def local[A](f: E => E)(fa: ReaderT[F, E, A]) = ReaderT[F, E, A](e => fa.runT(f(e)))
}

trait ReaderTMonadTrans[E] extends MonadTrans[({type f[g[_], a] = ReaderT[g, E, a]})#f] {
  def liftM[G[_]: Monad, A](ga: G[A]) = ReaderT[G, E, A](e => Monad[G].map(ga)(a => a))

  def hoist[M[_], N[_]](f: M ~> N) = new (({type f[x] = ReaderT[M, E, x]})#f ~> ({type f[x] = ReaderT[N, E, x]})#f) {
    def apply[A](fa: ReaderT[M, E, A]): ReaderT[N, E, A] = ReaderT[N, E, A](e => f(fa.runT(e)))
  }
}
