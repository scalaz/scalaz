package scalaz

trait TraverseTImpl[T[_[_], _], N[_], M[_], A] {
  val run: M[N[A]]
  def T: TraverseTWrapper[T, N]

  def map[B](f: A => B)(implicit M: Functor[M]): T[M, B] =
    T.wrap(M.map(run)(T.TraverseN.map(_)(f)))

  def flatMap[B](f: A => T[M, B])(implicit M: Monad[M], N: Monad[N]): T[M, B] =
    T.wrap(M.join(M.map(run) { na => M.map(T.TraverseN.traverseImpl(na) { a => T.unwrap(f(a)) })(N.join) }))

  def flatMapF[B](f: A => M[B])(implicit M: Monad[M]): T[M, B] =
    T.wrap(M.join(M.map(run) { na => T.TraverseN.traverseImpl(na)(f) }))

  def ap[B](f: => T[M, A => B])(implicit M: Apply[M], N: Apply[N]): T[M, B] =
    T.wrap(M.ap(run)(M.map(T.unwrap(f)) { (nf: N[A => B]) =>
      { (na: N[A]) => N.ap(na)(nf) }
    }))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit M: Foldable[M]): Z =
    M.foldRight[N[A], Z](run, z) { (a, b) => T.TraverseN.foldRight[A, Z](a, b)(f) }

  def traverse[G[_], B](f: A => G[B])(implicit M: Traverse[M], G: Applicative[G]): G[T[M, B]] =
    G.map(M.traverse(run) { na => T.TraverseN.traverse(na)(f) })(T.wrap(_))

  def exists(f: A => Boolean)(implicit M: Functor[M]): M[Boolean] =
    M.map(run)(T.TraverseN.any(_)(f))

  def forall(f: A => Boolean)(implicit M: Functor[M]): M[Boolean] =
    M.map(run)(T.TraverseN.all(_)(f))
}

object TraverseTImpl {

  trait `(*->*)->*->*`[T[_[_], _], N[_]]
    extends `(*->*)->*->*`.Instances[T, N] with `(*->*)->*->*`.Functions[T, N]

  object `(*->*)->*->*` {
    sealed trait Instances5[T[_[_], _], N[_]] { self =>
      def T: TraverseTWrapper[T, N]

      implicit def traverseTFunctor[M[_]](implicit M0: Functor[M]): Functor[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTFunctor[T, N, M] {
          def T = self.T
          def N = self.T.TraverseN
          def M = M0
        }
    }

    sealed trait Instances4[T[_[_], _], N[_]] extends Instances5[T, N] { self =>
      implicit def traverseTApply[M[_]](implicit N0: Apply[N], M0: Apply[M]): Apply[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTApply[T, N, M] {
          def T = self.T
          def N = N0
          def M = M0
        }
    }

    sealed trait Instances3[T[_[_], _], N[_]] extends Instances4[T, N] { self =>
      implicit def traverseTApplicative[M[_]](implicit N0: Applicative[N], M0: Applicative[M]): Applicative[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTApplicative[T, N, M] {
          def T = self.T
          def N = N0
          def M = M0
        }
    }

    sealed trait Instances2[T[_[_], _], N[_]] extends Instances3[T, N] { self =>
      implicit def traverseTMonad[M[_]](implicit N0: Monad[N], M0: Monad[M]): Monad[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTMonad[T, N, M] {
          def T = self.T
          def N = N0
          def M = M0
        }
    }

    sealed trait Instances1[T[_[_], _], N[_]] extends Instances2[T, N] { self =>
      implicit def traverseTMonadPlus[M[_]](implicit N0: MonadPlus[N], M0: Monad[M]): MonadPlus[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTMonadPlus[T, N, M] {
          def T = self.T
          def N = N0
          def M = M0
        }

      implicit def traverseTFoldable[M[_]](implicit M0: Foldable[M]): Foldable[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTFoldable[T, N, M] {
          def T = self.T
          def M = M0
        }
    }

    trait Instances[T[_[_], _], N[_]] extends Instances1[T, N] { self =>
      implicit def traverseTMonadTrans(implicit N0: MonadPlus[N]): Hoist[T] = new TraverseTHoist[T, N] {
        def T = self.T
        def N = N0
      }

      implicit def traverseTTraverse[M[_]](implicit M0: Traverse[M]): Traverse[({ type λ[α] = T[M, α] })#λ] =
        new TraverseTTraverse[T, N, M] {
          def T = self.T
          def M = M0
        }
    }

    trait Functions[T[_[_], _], N[_]] {
      def T: TraverseTWrapper[T, N]

      def traverseT[M[_]] = new (({type λ[α] = M[N[α]]})#λ ~> ({type λ[α] = T[M, α]})#λ) {
        def apply[A](a: M[N[A]]): T[M, A] = T.wrap(a)
      }
    }

  }


}

trait TraverseTWrapper[T[_[_], _], N[_]] {
  def wrap[M[_], A](mna: M[N[A]]): T[M, A]
  def unwrap[M[_], A](t: T[M, A]): M[N[A]]
  def TraverseN: Traverse[N]
}

trait TraverseTFunctor[T[_[_], A], N[_], M[_]] extends Functor[({ type λ[α] = T[M, α] })#λ] {
  implicit def T: TraverseTWrapper[T, N]
  implicit def M: Functor[M]
  implicit def N: Functor[N]

  override def map[A, B](mna: T[M, A])(f: A => B): T[M, B] =
    T.wrap(M.map(T.unwrap(mna))(N.map(_)(f)))
}

trait TraverseTApply[T[_[_], _], N[_], M[_]] extends TraverseTFunctor[T, N, M] with Apply[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Apply[M]
  implicit def N: Apply[N]

  def ap[A,B](ta: => T[M, A])(f: => T[M, A => B]): T[M, B] =
    T.wrap(M.ap(T.unwrap(ta))(M.map(T.unwrap(f)) { (nf: N[A => B]) =>
      { (na: N[A]) => N.ap(na)(nf) }
    }))
}

trait TraverseTApplicative[T[_[_], _], N[_], M[_]] extends TraverseTApply[T, N, M] with Applicative[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Applicative[M]
  implicit def N: Applicative[N]

  def point[A](a: => A): T[M, A] = T.wrap(M.point(N.point(a)))
}

trait TraverseTMonad[T[_[_], _], N[_], M[_]] extends TraverseTApplicative[T, N, M] with Monad[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Monad[M]
  implicit def N: Monad[N]

  def bind[A, B](fa: T[M, A])(f: A => T[M, B]): T[M, B] =
    T.wrap(M.join(M.map(T.unwrap(fa)) { na =>
      M.map(T.TraverseN.traverseImpl(na) { a => T.unwrap(f(a)) })(N.join)
    }))
}

trait TraverseTFoldable[T[_[_], _], N[_], M[_]] extends Foldable.FromFoldr[({ type λ[α] = T[M, α] })#λ] {
  implicit def T: TraverseTWrapper[T, N]
  implicit def M: Foldable[M]
  implicit def N = T.TraverseN

  override def foldRight[A, B](ta: T[M, A], z: => B)(f: (A, => B) => B): B =
    M.foldRight[N[A], B](T.unwrap(ta), z) { (a, b) => N.foldRight[A, B](a, b)(f) }
}

trait TraverseTTraverse[T[_[_], _], N[_], M[_]] extends Traverse[({ type λ[α] = T[M, α] })#λ]
    with TraverseTFunctor[T, N, M] with TraverseTFoldable[T, N, M] {
  implicit def M: Traverse[M]
  implicit def N: Traverse[N]

  def traverseImpl[G[_]: Applicative, A, B](ta: T[M, A])(f: A => G[B]): G[T[M, B]] =
    Functor[G].map(M.traverse(T.unwrap(ta)) { na => N.traverse(na)(f) })(T.wrap(_))
}

trait TraverseTHoist[T[_[_], _], N[_]] extends Hoist[({ type λ[M[_], α] = T[M, α] })#λ] { self =>
  implicit def T: TraverseTWrapper[T, N]
  implicit def N: Monad[N]

  def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): T[M, A] =
    T.wrap(M.map(ma) { a => N.point(a) })

  def hoist[M0[_]: Monad, M1[_]](f: M0 ~> M1) =
    new (({ type λ[α] = T[M0, α] })#λ ~> ({ type λ[α] = T[M1, α] })#λ) {
      def apply[A](ta: T[M0, A]): T[M1, A] = T.wrap(f.apply(T.unwrap(ta)))
    }

  implicit def apply[M[_]](implicit M0: Monad[M]): Monad[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonad[T, N, M] {
      val T = self.T
      val M = M0
      val N = self.N
    }
}

trait TraverseTMonadPlus[T[_[_], _], N[_], M[_]] extends MonadPlus[({ type λ[α] = T[M, α] })#λ]
    with TraverseTMonad[T, N, M] {
  implicit def M: Monad[M]
  implicit def N: MonadPlus[N]

  def empty[A]: T[M, A] = T.wrap(M point N.empty[A])
  def plus[A](a: T[M, A], b: => T[M, A]): T[M, A] =
    T.wrap(M.apply2(T.unwrap(a), T.unwrap(b)) { (na, nb) => N.plus[A](na, nb) })
}

// vim: expandtab:ts=2:sw=2
