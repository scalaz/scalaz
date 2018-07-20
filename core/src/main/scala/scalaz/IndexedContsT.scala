package scalaz

final case class IndexedContsT[W[_], R, O, M[_], A](_run: W[A => M[O]] => M[R]) {
  def run(wamo: W[A => M[O]]): M[R] =
    _run(wamo)

  def apply(wamo: W[A => M[O]]): M[R] =
    run(wamo)

  def run_(implicit W: Applicative[W], M: Applicative[M], ev: A === O): M[R] =
    run(W.point(x => M.point(ev(x))))

  def map[B](f: A => B)(implicit W: Functor[W]): IndexedContsT[W, R, O, M, B] =
    IndexedContsT { wbmo =>
      run(W.map(wbmo)(f andThen _))
    }

  def flatten[E, B](implicit ev: A === IndexedContsT[W, O, E, M, B], W: Cobind[W]): IndexedContsT[W, R, E, M, B] = flatMap(ev)

  def flatMap[E, B](f: A => IndexedContsT[W, O, E, M, B])(implicit W: Cobind[W]): IndexedContsT[W, R, E, M, B] =
    IndexedContsT { wbme =>
      run(W.cobind(wbme) { wk => { a =>
        f(a).run(wk)
      } })
    }

  def contramap[I](f: I => O)(implicit M: Functor[M], W: Functor[W]): IndexedContsT[W, R, I, M, A] =
    IndexedContsT { wami =>
      run(W.map(wami) { ami => { a => M.map(ami(a))(f) } })
    }

  def imap[E](f: R => E)(implicit M: Functor[M]): IndexedContsT[W, E, O, M, A] =
    IndexedContsT { wamo =>
      M.map(run(wamo))(f)
    }

  def bimap[E, B](f: R => E, g: A => B)(implicit M: Functor[M], W: Functor[W]): IndexedContsT[W, E, O, M, B] =
    IndexedContsT { wbmo =>
      M.map(run(W.map(wbmo)(g andThen _)))(f)
    }

  def xmap[E, I](f: R => E, g: I => O)(implicit M: Functor[M], W: Functor[W]): IndexedContsT[W, E, I, M, A] =
    IndexedContsT { wami =>
      M.map(run(W.map(wami) { ami => { a => M.map(ami(a))(g) } }))(f)
    }

  import BijectionT._

  def bmap[X >: R <: O, Z](f: Bijection[X, Z])(implicit M: Functor[M], W: Functor[W]): ContsT[W, Z, M, A] =
    IndexedContsT { wami =>
      M.map(run(W.map(wami) { ami => { a => M.map(ami(a))(f from _) } }))(f to _)
    }

  def plus(that: IndexedContsT[W, R, O, M, A])(implicit M: Plus[M]): IndexedContsT[W, R, O, M, A] =
    IndexedContsT { wamo =>
      M.plus(this(wamo), that(wamo))
    }
}

object IndexedContsT extends IndexedContsTInstances with IndexedContsTFunctions

trait IndexedContsTFunctions {
  def point[W[_], R, M[_], A](a: => A)(implicit W: Comonad[W]): ContsT[W, R, M, A] =
    ContsT { k => W.copoint(k)(a) }

  def empty[W[_], R, M[_], A](implicit M: PlusEmpty[M]): ContsT[W, R, M, A] =
    ContsT { k => M.empty }

  def liftM[W[_], R, M[_], A](a: => M[A])(implicit W: Comonad[W], M: Bind[M]): ContsT[W, R, M, A] =
    ContsT { k => M.bind(a)(W.copoint(k)) }

  def xhoist[W[_], R, O, M[_], N[_]](f: M ~> N, g: N ~> M)(implicit W: Functor[W]): IndexedContsT[W, R, O, M, ?] ~> IndexedContsT[W, R, O, N, ?] =
    λ[IndexedContsT[W, R, O, M, ?] ~> IndexedContsT[W, R, O, N, ?]](
      fa => IndexedContsT { wk => f(fa.run(W.map(wk) { k => { x => g(k(x)) } })) }
    )

  def contracohoist[W[_], V[_], R, O, M[_]](f: V ~> W): (IndexedContsT[W, R, O, M, ?] ~> IndexedContsT[V, R, O, M, ?]) =
    λ[IndexedContsT[W, R, O, M, ?] ~> IndexedContsT[V, R, O, M, ?]](
      fa => IndexedContsT { k => fa.run(f(k)) }
    )

  def shift[W[_], I, R, J, O, M[_], A](f: (A => IndexedContsT[W, I, I, M, O]) => IndexedContsT[W, R, J, M, J])(implicit W: Comonad[W], WA: Applicative[W], M: Monad[M]): IndexedContsT[W, R, O, M, A] =
    IndexedContsT { k0 =>
      (f { a =>
        IndexedContsT { k1 =>
          M.bind(W.copoint(k0)(a))(W.copoint(k1))
        }
      }).run_
    }

  def reset[W[_], R, O, M[_], A](v: IndexedContsT[W, A, O, M, O])(implicit W: Comonad[W], WA: Applicative[W], M: Monad[M]): IndexedContsT[W, R, R, M, A] =
    IndexedContsT { k =>
      M.bind(v.run_)(W.copoint(k))
    }

  def callCC[W[_], R, O, M[_], A, B](f: (A => IndexedContsT[W, O, O, M, B]) => IndexedContsT[W, R, O, M, A])(implicit W: Comonad[W]): IndexedContsT[W, R, O, M, A] =
    IndexedContsT { k =>
      (f { a =>
        IndexedContsT[W, O, O, M, B] { _ =>
          W.copoint(k)(a)
        }
      }).run(k)
    }
}

sealed abstract class IndexedContsTInstances2 {
  implicit def IndexedContsTFunctorRight[W[_], R, M[_], O](implicit W0: Functor[W]): Functor[IndexedContsT[W, R, O, M, ?]] =
    new IndexedContsTFunctorRight[W, M, R, O] {
      implicit val W: Functor[W] = W0
    }
}

sealed abstract class IndexedContsTInstances1 extends IndexedContsTInstances2 {
  implicit def ContsTBind[W[_], R, M[_]](implicit W0: Cobind[W]): Bind[ContsT[W, R, M, ?]] =
    new ContsTBind[W, M, R] {
      val W = W0
    }
}

sealed abstract class IndexedContsTInstances0 extends IndexedContsTInstances1 {
  implicit def ContsTMonad[W[_], R, M[_]](implicit W0: Comonad[W]): Monad[ContsT[W, R, M, ?]] =
    new ContsTMonad[W, M, R] {
      implicit val W: Comonad[W] = W0
    }
}

abstract class IndexedContsTInstances extends IndexedContsTInstances0 {
  implicit def IndexedContsTFunctorLeft[W[_], O, M[_], A](implicit M0: Functor[M]): Functor[IndexedContsT[W, ?, O, M, A]] =
    new IndexedContsTFunctorLeft[W, M, O, A] {
      implicit val M: Functor[M] = M0
    }

  implicit def IndexedContsTContravariant[W[_], R, M[_], A](implicit W0: Functor[W], M0: Functor[M]): Contravariant[IndexedContsT[W, R, ?, M, A]] =
    new IndexedContsTContravariant[W, M, R, A] {
      implicit val W: Functor[W] = W0
      implicit val M: Functor[M] = M0
    }

  implicit def IndexedContsTBifunctor[W[_], O, M[_]](implicit W0: Functor[W], M0: Functor[M]): Bifunctor[IndexedContsT[W, ?, O, M, ?]] =
    new IndexedContsTBifunctor[W, M, O] {
      implicit val W: Functor[W] = W0
      implicit val M: Functor[M] = M0
    }

  implicit def ContsTMonadPlusAlt[W[_], R, M[_]](implicit W0: Comonad[W], M0: PlusEmpty[M]): MonadPlus[ContsT[W, R, M, ?]] with Alt[ContsT[W, R, M, ?]] =
    new ContsTMonadPlus[W, M, R] with Alt[ContsT[W, R, M, ?]] {
      implicit val W: Comonad[W] = W0
      implicit val M: PlusEmpty[M] = M0

      @inline
      override def alt[A](a: => ContsT[W, R, M, A], b: => ContsT[W, R, M, A]): ContsT[W, R, M, A] =
        plus(a, b)
    }
}

private sealed trait IndexedContsTFunctorLeft[W[_], M[_], O, A0] extends Functor[IndexedContsT[W, ?, O, M, A0]] {
  implicit val M: Functor[M]

  def map[A, B](fa: IndexedContsT[W, A, O, M, A0])(f: A => B): IndexedContsT[W, B, O, M, A0] = fa.imap(f)
}

private sealed trait IndexedContsTFunctorRight[W[_], M[_], R, O] extends Functor[IndexedContsT[W, R, O, M, ?]] {
  implicit val W: Functor[W]

  override def map[A, B](fa: IndexedContsT[W, R, O, M, A])(f: A => B): IndexedContsT[W, R, O, M, B] = fa.map(f)
}

private sealed trait IndexedContsTContravariant[W[_], M[_], R, A0] extends Contravariant[IndexedContsT[W, R, ?, M, A0]] {
  implicit val W: Functor[W]
  implicit val M: Functor[M]

  def contramap[A, B](fa: IndexedContsT[W, R, A, M, A0])(f: B => A): IndexedContsT[W, R, B, M, A0] = fa.contramap(f)
}

private sealed trait IndexedContsTBifunctor[W[_], M[_], O] extends Bifunctor[IndexedContsT[W, ?, O, M, ?]] {
  implicit val W: Functor[W]
  implicit val M: Functor[M]

  def bimap[A, B, C, D](fab: IndexedContsT[W, A, O, M, B])(f: A => C, g: B => D): IndexedContsT[W, C, O, M, D] = fab.bimap(f, g)

  override def leftFunctor[X]: Functor[IndexedContsT[W, ?, O, M, X]] = IndexedContsT.IndexedContsTFunctorLeft

  override def leftMap[A, B, C](fa: IndexedContsT[W, A, O, M, B])(f: A => C): IndexedContsT[W, C, O, M, B] = fa.imap(f)

  override def rightFunctor[X]: Functor[IndexedContsT[W, X, O, M, ?]] = IndexedContsT.IndexedContsTFunctorRight

  override def rightMap[A, B, D](fa: IndexedContsT[W, A, O, M, B])(f: B => D): IndexedContsT[W, A, O, M, D] = fa.map(f)
}

private sealed trait ContsTBind[W[_], M[_], R] extends Bind[ContsT[W, R, M, ?]] with IndexedContsTFunctorRight[W, M, R, R] {
  implicit val W: Cobind[W]

  override def bind[A, B](fa: ContsT[W, R, M, A])(f: A => ContsT[W, R, M, B]) = fa.flatMap(f)

  override def join[A](ffa: ContsT[W, R, M, ContsT[W, R, M, A]]) = ffa.flatten
}

private sealed trait ContsTMonad[W[_], M[_], R] extends Monad[ContsT[W, R, M, ?]] with ContsTBind[W, M, R] {
  implicit val W: Comonad[W]

  override def point[A](a: => A) = IndexedContsT.point(a)
}

private sealed trait ContsTMonadPlus[W[_], M[_], R] extends MonadPlus[ContsT[W, R, M, ?]] with ContsTMonad[W, M, R] {
  implicit val M: PlusEmpty[M]

  override def empty[A]: ContsT[W, R, M, A] = IndexedContsT.empty

  override def plus[A](a: ContsT[W, R, M, A], b: => ContsT[W, R, M, A]): ContsT[W, R, M, A] = a.plus(b)
}
