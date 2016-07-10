package scalaz

final case class IdT[F[_], A](run: F[A]) {
  def map[B](f: A => B)(implicit F: Functor[F]) =
    new IdT[F, B](F.map(run)(f))

  def flatMap[B](f: A => IdT[F, B])(implicit F: Bind[F]) =
    new IdT[F, B](F.bind(run)(f andThen ((_: IdT[F, B]).run)))

  def flatMapF[B](f: A => F[B])(implicit F: Bind[F]) =
    new IdT[F, B](F.bind(run)(f))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[A, Z](run, z)(f)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[IdT[F, B]] =
    G.map(F.traverse(run)(f))(IdT(_))

  def ap[B](f: => IdT[F, A => B])(implicit F: Apply[F]) =
    new IdT(F.ap(run)(f.run))
}

sealed abstract class IdTInstances4 {
  implicit def idTFunctor[F[_]](implicit F0: Functor[F]): Functor[IdT[F, ?]] =
    new IdTFunctor[F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class IdTInstances3 extends IdTInstances4 {
  implicit def idTApply[F[_]](implicit F0: Apply[F]): Apply[IdT[F, ?]] =
    new IdTApply[F] {
      implicit def F: Apply[F] = F0
    }
}

sealed abstract class IdTInstances2 extends IdTInstances3 {
  implicit def idTApplicative[F[_]](implicit F0: Applicative[F]): Applicative[IdT[F, ?]] =
    new IdTApplicative[F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class IdTInstances1 extends IdTInstances2 {
  implicit def idTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[IdT[F, ?]] =
    new IdTFoldable[F] {
      implicit def F: Foldable[F] = F0
    }
  implicit def idTBindRec[F[_]](implicit F0: BindRec[F]): BindRec[IdT[F, ?]] =
    new IdTBindRec[F] {
      implicit def F: BindRec[F] = F0
    }
}

sealed abstract class IdTInstances0 extends IdTInstances1 {
  implicit def idTMonad[F[_]](implicit F0: Monad[F]): Monad[IdT[F, ?]] =
    new IdTMonad[F] {
      implicit def F: Monad[F] = F0
    }

  implicit def idTOrder[F[_], A](implicit F: Order[F[A]]): Order[IdT[F, A]] =
    F.contramap(_.run)
}

sealed abstract class IdTInstances extends IdTInstances0 {
  implicit val idTHoist: Hoist[IdT] = IdTHoist

  implicit def idTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[IdT[F, ?]] =
    new IdTTraverse[F] {
      implicit def F: Traverse[F] = F0
    }

  implicit def idTEqual[F[_], A](implicit F: Equal[F[A]]): Equal[IdT[F, A]] =
    F.contramap(_.run)
}

object IdT extends IdTInstances

//
// Implementation traits for type class instances
//

private trait IdTFunctor[F[_]] extends Functor[IdT[F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IdT[F, A])(f: A => B) = fa map f
}

private trait IdTApply[F[_]] extends Apply[IdT[F, ?]] with IdTFunctor[F] {
  implicit def F: Apply[F]

  override def ap[A, B](fa: => IdT[F, A])(f: => IdT[F, A => B]): IdT[F, B] = fa ap f
}

private trait IdTApplicative[F[_]] extends Applicative[IdT[F, ?]] with IdTApply[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A) = new IdT[F, A](F.point(a))
}

private trait IdTBind[F[_]] extends Bind[IdT[F, ?]] with IdTApply[F] {
  implicit def F: Bind[F]

  final def bind[A, B](fa: IdT[F, A])(f: A => IdT[F, B]) = fa flatMap f
}

private trait IdTBindRec[F[_]] extends BindRec[IdT[F, ?]] with IdTBind[F] {
  implicit def F: BindRec[F]

  final def tailrecM[A, B](f: A => IdT[F, A \/ B])(a: A): IdT[F, B] =
    IdT(F.tailrecM[A, B](a => F.map(f(a).run)(identity))(a))
}

private trait IdTMonad[F[_]] extends Monad[IdT[F, ?]] with IdTApplicative[F] with IdTBind[F] {
  implicit def F: Monad[F]
}

private trait IdTFoldable[F[_]] extends Foldable.FromFoldr[IdT[F, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: IdT[F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private trait IdTTraverse[F[_]] extends Traverse[IdT[F, ?]] with IdTFoldable[F] with IdTFunctor[F]{
  implicit def F: Traverse[F]

  def traverseImpl[G[_] : Applicative, A, B](fa: IdT[F, A])(f: A => G[B]): G[IdT[F, B]] = fa traverse f
}

private object IdTHoist extends Hoist[IdT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): IdT[G, A] =
    new IdT[G, A](a)

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    new (IdT[M, ?] ~> IdT[N, ?]) {
      def apply[A](fa: IdT[M, A]): IdT[N, A] =
        new IdT[N, A](f(fa.run))
    }

  implicit def apply[G[_] : Monad]: Monad[IdT[G, ?]] =
    IdT.idTMonad[G]
}

// vim: set ts=4 sw=4 et:
