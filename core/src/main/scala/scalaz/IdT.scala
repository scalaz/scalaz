package scalaz

final case class IdT[F[_], A](run: F[A]) {
  def map[B](f: A => B)(implicit F: Functor[F]) = new IdT[F, B](
    F.map(run)(f)
  )

  def flatMap[B](f: A => IdT[F, B])(implicit F: Monad[F]) = new IdT[F, B](
    F.bind(run)(f andThen ((_: IdT[F, B]).run))
  )

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]) = new IdT[F, B](
    F.bind(run)(f)
  )

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z = {
    F.foldRight[A, Z](run, z)(f)
  }

  def traverse[G[_], B](f: (A) => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[IdT[F, B]] = {
    import std.option._
    G.map(F.traverse(run)(f))(IdT(_))
  }

  def ap[B](f: => IdT[F, A => B])(implicit F: Apply[F]) = new IdT(
    F.ap(run)(f.run)
  )
}

trait IdTInstances2 {
  implicit def idTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = IdT[F, α]})#λ] = new IdTFunctor[F] {
    implicit def F: Functor[F] = F0
  }
}

trait IdTInstances1 extends IdTInstances2 {
  implicit def idTApply[F[_]](implicit F0: Apply[F]): Apply[({type λ[α] = IdT[F, α]})#λ] = new IdTApply[F] {
    implicit def F: Apply[F] = F0
  }
}

trait IdTInstances0 extends IdTInstances1 {
  implicit def idTApplicative[F[_]](implicit F0: Applicative[F]): Applicative[({type λ[α] = IdT[F, α]})#λ] = new IdTApplicative[F] {
    implicit def F: Applicative[F] = F0
  }

  implicit def idTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[({type λ[α] = IdT[F, α]})#λ] = new IdTFoldable[F] {
    implicit def F: Foldable[F] = F0
  }
}

trait IdTInstances extends IdTInstances0 {
  implicit def idTHoist: Hoist[IdT] = IdTHoist

  implicit def idTMonad[F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = IdT[F, α]})#λ] = new IdTMonad[F] {
    implicit def F: Monad[F] = F0
  }

  implicit def idTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[({type λ[α] = IdT[F, α]})#λ] = new IdTTraverse[F] {
    implicit def F: Traverse[F] = F0
  }
}

trait IdTFunctions {
}

object IdT extends IdTFunctions with IdTInstances 

//
// Implementation traits for type class instances
//

private[scalaz] trait IdTFunctor[F[_]] extends Functor[({type λ[α] = IdT[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IdT[F, A])(f: A => B) = fa map f
}

private[scalaz] trait IdTApply[F[_]] extends Apply[({type λ[α] = IdT[F, α]})#λ] with IdTFunctor[F] {
  implicit def F: Apply[F]

  override def ap[A, B](fa: => IdT[F, A])(f: => IdT[F, A => B]): IdT[F, B] = fa ap f
}

private[scalaz] trait IdTApplicative[F[_]] extends Applicative[({type λ[α] = IdT[F, α]})#λ] with IdTApply[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A) = new IdT[F, A](F.point(a))
}

private[scalaz] trait IdTMonad[F[_]] extends Monad[({type λ[α] = IdT[F, α]})#λ] with IdTApplicative[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: IdT[F, A])(f: A => IdT[F, B]) = fa flatMap f
}

private[scalaz] trait IdTFoldable[F[_]] extends Foldable.FromFoldr[({type λ[α] = IdT[F, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: IdT[F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private[scalaz] trait IdTTraverse[F[_]] extends Traverse[({type λ[α] = IdT[F, α]})#λ] with IdTFoldable[F] with IdTFunctor[F]{
  implicit def F: Traverse[F]

  def traverseImpl[G[_] : Applicative, A, B](fa: IdT[F, A])(f: (A) => G[B]): G[IdT[F, B]] = fa traverse f
}

private[scalaz] object IdTHoist extends Hoist[IdT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): IdT[G, A] = new IdT[G, A](a)

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = IdT[M, x]})#f ~> ({type f[x] = IdT[N, x]})#f) {
    def apply[A](fa: IdT[M, A]): IdT[N, A] = new IdT[N, A](
      f(fa.run)
    )
  }

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = IdT[G, α]})#λ] = IdT.idTMonad[G]
}

// vim: set ts=4 sw=4 et:
