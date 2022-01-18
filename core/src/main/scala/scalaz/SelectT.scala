package scalaz

import scalaz.Id.Id

/** Selection monad transformer.
  *
  * @see [[https://www.cs.bham.ac.uk/~mhe/papers/selection-escardo-oliva.pdf]]
  * @see [[https://arxiv.org/pdf/1406.2058.pdf]]
  * @see [[https://github.com/ghc/packages-transformers/blob/0_5_4_0/Control/Monad/Trans/Select.hs]]
  */
final case class SelectT[R, M[_], A](run: (A => M[R]) => M[A]) { self =>

  def map[B](f: A => B)(implicit F: Functor[M]): SelectT[R, M, B] =
    SelectT(x =>
      run.andThen(F.map(_)(f)).apply(x.compose(f))
    )

  def flatMap[B](f: A => SelectT[R, M, B])(implicit F: Bind[M]): SelectT[R, M, B] =
    SelectT { k =>
      val h = f.andThen(_.run(k))
      F.bind(run(h.andThen(F.bind(_)(k))))(h)
    }

  def toContT(implicit F: Bind[M]): ContT[M, R, A] =
    ContT(k => F.bind(run(k))(k))

}

sealed abstract class SelectTInstances7 {
  implicit def selectTFunctor[R, M[_]: Functor]: Functor[SelectT[R, M, *]] =
    new SelectTFunctor[R, M] {
      override def F = implicitly
    }
}

sealed abstract class SelectTInstances6 extends SelectTInstances7 {
  implicit def selectTBind[R, M[_]: Bind]: Bind[SelectT[R, M, *]] =
    new SelectTBind[R, M] {
      override def F = implicitly
    }
}

sealed abstract class SelectTInstances5 extends SelectTInstances6 {
  implicit def selectTPlus[R, M[_]: Plus]: Plus[SelectT[R, M, *]] =
    new SelectTPlus[R, M] {
      override def F = implicitly
    }
}

sealed abstract class SelectTInstances4 extends SelectTInstances5 {
  implicit def selectTPlusEmpty[R, M[_]: PlusEmpty]: PlusEmpty[SelectT[R, M, *]] =
    new SelectTPlusEmpty[R, M] {
      override def F = implicitly
    }
}

sealed abstract class SelectTInstances3 extends SelectTInstances4 {
  implicit def selectTMonad[R, M[_]: Monad]: Monad[SelectT[R, M, *]] =
    new SelectTMonad[R, M] {
      override def F = implicitly
    }
}

sealed abstract class SelectTInstances2 extends SelectTInstances3 {
  implicit def selectMonad[R]: Monad[Select[R, *]] =
    SelectT.selectTMonad[R, Id](Id.id)
}

sealed abstract class SelectTInstances1 extends SelectTInstances2 {
  implicit def selectTMonadPlus[R, M[_]: MonadPlus]: MonadPlus[SelectT[R, M, *]] =
    new SelectTMonadPlus[R, M] {
      override def F = implicitly
    }
}

sealed abstract class SelectTInstances extends SelectTInstances1 {
  implicit def selectTMonadTrans[R]: MonadTrans[({type l[x[_], y] = SelectT[R, x, y]})#l] =
    new MonadTrans[({type l[x[_], y] = SelectT[R, x, y]})#l] {
      override def liftM[G[_]: Monad, A](a: G[A]) =
        SelectT(_ => a)

      override def apply[G[_]: Monad] =
        SelectT.selectTMonad[R, G]
    }
}

object SelectT extends SelectTInstances {

}

private trait SelectTFunctor[R, M[_]] extends Functor[SelectT[R, M, *]] {
  protected implicit def F: Functor[M]

  override final def map[A, B](fa: SelectT[R, M, A])(f: A => B) =
    fa map f
}

private trait SelectTBind[R, M[_]] extends Bind[SelectT[R, M, *]] with SelectTFunctor[R, M] {
  protected implicit def F: Bind[M]

  override def bind[A, B](fa: SelectT[R, M, A])(f: A => SelectT[R, M, B]) =
    fa flatMap f
}

private trait SelectTMonad[R, M[_]] extends Monad[SelectT[R, M, *]] with SelectTBind[R, M] {
  protected def F: Monad[M]

  override def point[A](a: => A): SelectT[R, M, A] =
    SelectT(_ => F.point(a))
}

private trait SelectTPlus[R, M[_]] extends Plus[SelectT[R, M, *]] {
  protected def F: Plus[M]

  override def plus[A](a: SelectT[R, M, A], b: => SelectT[R, M, A]) =
    SelectT(k => F.plus(a.run(k), b.run(k)))
}

private trait SelectTPlusEmpty[R, M[_]] extends PlusEmpty[SelectT[R, M, *]] with SelectTPlus[R, M] {
  protected def F: PlusEmpty[M]

  override def empty[A] =
    SelectT(_ => F.empty[A])
}

private trait SelectTMonadPlus[R, M[_]] extends MonadPlus[SelectT[R, M, *]] with SelectTPlusEmpty[R, M] with SelectTMonad[R, M] {
  protected def F: MonadPlus[M]
}
