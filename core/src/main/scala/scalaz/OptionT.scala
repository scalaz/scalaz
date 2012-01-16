package scalaz

import std.option.optionInstance

/**
 * OptionT monad transformer.
 */
final case class OptionT[F[_], A](run: F[Option[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = new OptionT[F, B](
    F.map(run)(_ map f)
  )

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.bind(self.run) {
      case None    => F.point(None: Option[B])
      case Some(z) => f(z).run
    }
  )

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.bind(self.run) {
      case None    => F.point(None: Option[B])
      case Some(z) => F.map(f(z))(b => Some(b))
    }
  )

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z = {
    import std.option._
    F.foldRight[Option[A], Z](run, z)((a, b) => Foldable[Option].foldRight[A, Z](a, b)(f))
  }

  def traverse[G[_], B](f: (A) => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] = {
    import std.option._
    G.map(F.traverse(run)(o => Traverse[Option].traverse(o)(f)))(OptionT(_))
  }

  def ap[B](f: => OptionT[F, A => B])(implicit F: Apply[F]): OptionT[F, B] =
    OptionT(F.map2(f.run, run) {
      case (ff, aa) => optionInstance.ap(aa)(ff)
    })
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
  implicit def optionTApply[F[_]](implicit F0: Apply[F]): Apply[({type λ[α] = OptionT[F, α]})#λ] = new OptionTApply[F] {
    implicit def F: Apply[F] = F0
  }
}

trait OptionTInstances0 extends OptionTInstances1 {
  implicit def optionTAlternative[F[_]](implicit F0: Alternative[F]): Alternative[({type λ[α] = OptionT[F, α]})#λ] = new OptionTAlternative[F] {
    implicit def F: Alternative[F] = F0
  }
  implicit def optionTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[({type λ[α] = OptionT[F, α]})#λ] = new OptionTFoldable[F] {
    implicit def F: Foldable[F] = F0
  }
}

trait OptionTInstances extends OptionTInstances0 {
  implicit def optionTMonadTrans: MonadTrans[OptionT] = new OptionTMonadTrans {}

  implicit def optionTMonad[F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = OptionT[F, α]})#λ] = new OptionTMonad[F] {
    implicit def F: Monad[F] = F0
  }

  implicit def optionTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[({type λ[α] = OptionT[F, α]})#λ] = new OptionTTraverse[F] {
    implicit def F: Traverse[F] = F0
  }

  implicit def optionTEqual[F[_], A](implicit F0: Equal[F[Option[A]]]): Equal[OptionT[F, A]] = F0.contramap((_: OptionT[F, A]).run)
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

  def ap[A, B](fa: => OptionT[F, A])(f: => OptionT[F, A => B]): OptionT[F, B] = fa ap f
}

private[scalaz] trait OptionTMonad[F[_]] extends Monad[({type λ[α] = OptionT[F, α]})#λ] with OptionTPointed[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa flatMap f
}

private[scalaz] trait OptionTFoldable[F[_]] extends Foldable.FromFoldr[({type λ[α] = OptionT[F, α]})#λ] {
  implicit def F: Foldable[F]

  def foldRight[A, B](fa: OptionT[F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private[scalaz] trait OptionTTraverse[F[_]] extends Traverse[({type λ[α] = OptionT[F, α]})#λ] with OptionTFoldable[F] with OptionTFunctor[F]{
  implicit def F: Traverse[F]

  def traverseImpl[G[_] : Applicative, A, B](fa: OptionT[F, A])(f: (A) => G[B]): G[OptionT[F, B]] = fa traverse f
}

trait OptionTAlternative[F[_]] extends Alternative[({type λ[α] = OptionT[F, α]})#λ] with OptionTApply[F] with OptionTPointed[F] {
  implicit def F: Alternative[F]
  
  def orElse[A](a: OptionT[F, A], b: => OptionT[F, A]): OptionT[F, A] = OptionT(F.orElse(a.run, b.run))
}

private[scalaz] trait OptionTMonadTrans extends MonadTrans[OptionT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
    OptionT[G, A](G.map[A, Option[A]](a)((a: A) => Some(a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = OptionT[M, x]})#f ~> ({type f[x] = OptionT[N, x]})#f) {
    def apply[A](fa: OptionT[M, A]): OptionT[N, A] = OptionT(f.apply(fa.run))
  }

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = OptionT[G, α]})#λ] = OptionT.optionTMonad[G]
}
