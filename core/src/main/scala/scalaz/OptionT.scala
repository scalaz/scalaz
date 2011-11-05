package scalaz

import std.option.optionInstance

/**
 * OptionT monad transformer.
 */
final case class OptionT[F[_], A](value: F[Option[A]]) {
  self =>
  
  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = new OptionT[F, B](
    F.map(value)(_ map f)
  )

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.bind(self.value) {
      case None => F.point(None: Option[B])
      case Some(z) => f(z).value
    }
  )

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.bind(self.value) {
      case None => F.point(None: Option[B])
      case Some(z) => F.map(f(z))(b => Some(b))
    }
  )
}

//
// Prioritized Implicits for type class instances
//

trait OptionTInstances2 {
  implicit def optionTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = OptionT[F, α]})#λ] = new OptionTFunctor[F] {
    implicit def F: Functor[F] = F0
  }
}

trait OptionTInstances1 extends OptionTInstances2 {
  implicit def optionTPointed[F[_]](implicit F0: Pointed[F]): Pointed[({type λ[α] = OptionT[F, α]})#λ] = new OptionTPointed[F] {
    implicit def F: Pointed[F] = F0
  }
}

trait OptionTInstances0 extends OptionTInstances1 {
  implicit def optionTApply[F[_]](implicit F0: Apply[F]): Apply[({type λ[α] = OptionT[F, α]})#λ] = new OptionTApply[F] {
      implicit def F: Apply[F] = F0
    }
}

trait OptionTInstances extends OptionTInstances0 {
  implicit def optionTMonadTrans: MonadTrans[OptionT] = new OptionTMonadTrans {}

  implicit def optionTMonad[F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = OptionT[F, α]})#λ] = new OptionTMonad[F] {
    implicit def F: Monad[F] = F0
  }
}

trait OptionTFunctions {
  def optionT[M[_]] = new (({type λ[α] = M[Option[α]]})#λ ~> ({type λ[α] = OptionT[M, α]})#λ) {
    def apply[A](a: M[Option[A]]) = new OptionT[M, A](a)
  }
}

object OptionT extends OptionTFunctions with OptionTInstances

//
// Implementation traits for type class instances
//

private[scalaz] trait OptionTFunctor[F[_]] extends Functor[({type λ[α] = OptionT[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa map f
}

private[scalaz] trait OptionTPointed[F[_]] extends Pointed[({type λ[α] = OptionT[F, α]})#λ] with OptionTFunctor[F] {
  implicit def F: Pointed[F]

  def point[A](a: => A): OptionT[F, A] = OptionT[F, A](F.point(Some(a)))
}

private[scalaz] trait OptionTApply[F[_]] extends Apply[({type λ[α] = OptionT[F, α]})#λ] with OptionTFunctor[F] {
  implicit def F: Apply[F]

  def ap[A, B](fa: OptionT[F, A])(f: OptionT[F, A => B]): OptionT[F, B] =
    OptionT(F.map2(f.value, fa.value)({ case (ff, aa) => optionInstance.ap(aa)(ff) }))
}

private[scalaz] trait OptionTMonad[F[_]] extends Monad[({type λ[α] = OptionT[F, α]})#λ] with OptionTPointed[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa flatMap f
}

private[scalaz] trait OptionTMonadTrans extends MonadTrans[OptionT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
    OptionT[G, A](G.map[A, Option[A]](a)((a: A) => Some(a)))

  def hoist[M[_], N[_]](f: M ~> N) = new (({type f[x] = OptionT[M, x]})#f ~> ({type f[x] = OptionT[N, x]})#f) {
    def apply[A](fa: OptionT[M, A]): OptionT[N, A] = OptionT(f.apply(fa.value))
  }
}
