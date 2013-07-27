package scalaz

sealed abstract class IndexedContsT[W[_], M[_], R, O, A] {
  def run(wamo: W[A => M[O]]): M[R]

  def apply(wamo: W[A => M[O]]): M[R] = run(wamo)

  def run_(implicit W: Applicative[W], M: Applicative[M], ev: A <:< O): M[R] = run(W.point(x => M.point(x)))

  def map[B](f: A => B)(implicit W: Functor[W]): IndexedContsT[W, M, R, O, B] = IndexedContsT { wbmo =>
    run(W.map(wbmo)(f andThen _))
  }

  def flatten[E, B](implicit ev: A <:< IndexedContsT[W, M, O, E, B], W: Cobind[W]): IndexedContsT[W, M, R, E, B] = flatMap(ev)

  def flatMap[E, B](f: A => IndexedContsT[W, M, O, E, B])(implicit W: Cobind[W]): IndexedContsT[W, M, R, E, B] = IndexedContsT { wbme =>
    run(W.cobind(wbme) { wk => { a =>
      f(a).run(wk)
    } })
  }
  
  def contramap[I](f: I => O)(implicit M: Functor[M], W: Functor[W]): IndexedContsT[W, M, R, I, A] = IndexedContsT { wami =>
    run(W.map(wami) { ami => { a => M.map(ami(a))(f) } })
  }

  def imap[E](f: R => E)(implicit M: Functor[M]): IndexedContsT[W, M, E, O, A] = IndexedContsT { wamo =>
    M.map(run(wamo))(f)
  }

  def bimap[E, B](f: R => E, g: A => B)(implicit M: Functor[M], W: Functor[W]): IndexedContsT[W, M, E, O, B] = IndexedContsT { wbmo =>
    M.map(run(W.map(wbmo)(g andThen _)))(f)
  }

  def xmap[E, I](f: R => E, g: I => O)(implicit M: Functor[M], W: Functor[W]): IndexedContsT[W, M, E, I, A] = IndexedContsT { wami =>
    M.map(run(W.map(wami) { ami => { a => M.map(ami(a))(g) } }))(f)
  }

  import BijectionT._

  def bmap[X >: R <: O, Z](f: Bijection[X, Z])(implicit M: Functor[M], W: Functor[W]): ContsT[W, M, Z, A] = IndexedContsT { wami =>
    M.map(run(W.map(wami) { ami => { a => M.map(ami(a))(f from _) } }))(f to _)
  }
}

object IndexedContsT extends IndexedContsTInstances with IndexedContsTFunctions {
  def apply[W[_], M[_], R, O, A](f: W[A => M[O]] => M[R]): IndexedContsT[W, M, R, O, A] = new IndexedContsT[W, M, R, O, A] {
    def run(wamo: W[A => M[O]]): M[R] = f(wamo)
  }
}

trait IndexedContsTFunctions {
  def point[W[_], M[_], R, A](a: => A)(implicit W: Comonad[W]): ContsT[W, M, R, A] = ContsT { k => W.copoint(k)(a) }

  def liftM[W[_], M[_], R, A](a: => M[A])(implicit W: Comonad[W], M: Bind[M]): ContsT[W, M, R, A] = ContsT { k => M.bind(a)(W.copoint(k)) }

  def xhoist[W[_], M[_], N[_], R, O](f: M ~> N, g: N ~> M)(implicit W: Functor[W]): (({type f[x]=IndexedContsT[W, M, R, O, x]})#f ~> ({type f[x]=IndexedContsT[W, N, R, O, x]})#f) = new (({type f[x]=IndexedContsT[W, M, R, O, x]})#f ~> ({type f[x]=IndexedContsT[W, N, R, O, x]})#f) {
    def apply[A](fa: IndexedContsT[W, M, R, O, A]): IndexedContsT[W, N, R, O, A] = IndexedContsT { wk => f(fa.run(W.map(wk) { k => { x => g(k(x)) } })) }
  }

  def contracohoist[W[_], V[_], M[_], R, O](f: V ~> W): (({type f[x]=IndexedContsT[W, M, R, O, x]})#f ~> ({type f[x]=IndexedContsT[V, M, R, O, x]})#f) = new (({type f[x]=IndexedContsT[W, M, R, O, x]})#f ~> ({type f[x]=IndexedContsT[V, M, R, O, x]})#f) {
    def apply[A](fa: IndexedContsT[W, M, R, O, A]): IndexedContsT[V, M, R, O, A] = IndexedContsT { k => fa.run(f(k)) }
  }

  def shift[W[_], M[_], I, R, J, O, A](f: (A => IndexedContsT[W, M, I, I, O]) => IndexedContsT[W, M, R, J, J])(implicit W: Comonad[W], WA: Applicative[W], M: Monad[M]): IndexedContsT[W, M, R, O, A] =
    IndexedContsT { k0 =>
      (f { a =>
        IndexedContsT { k1 =>
          M.bind(W.copoint(k0)(a))(W.copoint(k1))
        }
      }).run_
    }

  def reset[W[_], M[_], R, O, A](v: IndexedContsT[W, M, A, O, O])(implicit W: Comonad[W], WA: Applicative[W], M: Monad[M]): IndexedContsT[W, M, R, R, A] =
    IndexedContsT { k =>
      M.bind(v.run_)(W.copoint(k))
    }

  def callCC[W[_], M[_], R, O, A, B](f: (A => IndexedContsT[W, M, O, O, B]) => IndexedContsT[W, M, R, O, A])(implicit W: Comonad[W]): IndexedContsT[W, M, R, O, A] =
    IndexedContsT { k =>
      (f { a =>
        IndexedContsT[W, M, O, O, B] { _ =>
          W.copoint(k)(a)
        }
      }).run(k)
    }
}

sealed abstract class IndexedContsTInstances0 {
  implicit def IndexedContsTFunctorRight[W[_], M[_], R, O](implicit W0: Functor[W]): Functor[({type f[a]=IndexedContsT[W, M, R, O, a]})#f] = new IndexedContsTFunctorRight[W, M, R, O] {
    implicit val W: Functor[W] = W0
  }
}

abstract class IndexedContsTInstances extends IndexedContsTInstances0 {
  implicit def IndexedContsTFunctorLeft[W[_], M[_], O, A](implicit M0: Functor[M]): Functor[({type f[r]=IndexedContsT[W, M, r, O, A]})#f] = new IndexedContsTFunctorLeft[W, M, O, A] {
    implicit val M: Functor[M] = M0
  }

  implicit def IndexedContsTContravariant[W[_], M[_], R, A](implicit W0: Functor[W], M0: Functor[M]): Contravariant[({type f[o]=IndexedContsT[W, M, R, o, A]})#f] = new IndexedContsTContravariant[W, M, R, A] {
    implicit val W: Functor[W] = W0
    implicit val M: Functor[M] = M0
  }

  implicit def IndexedContsTBifunctor[W[_], M[_], O](implicit W0: Functor[W], M0: Functor[M]): Bifunctor[({type f[r, a]=IndexedContsT[W, M, r, O, a]})#f] = new IndexedContsTBifunctor[W, M, O] {
    implicit val W: Functor[W] = W0
    implicit val M: Functor[M] = M0
  }

  implicit def ContsTMonad[W[_], M[_], R](implicit W0: Comonad[W]): Monad[({type f[a]=ContsT[W, M, R, a]})#f] = new ContsTMonad[W, M, R] {
    implicit val W: Comonad[W] = W0
  }
}

private[scalaz] sealed trait IndexedContsTFunctorLeft[W[_], M[_], O, A0] extends Functor[({type f[r]=IndexedContsT[W, M, r, O, A0]})#f] {
  implicit val M: Functor[M]

  def map[A, B](fa: IndexedContsT[W, M, A, O, A0])(f: A => B): IndexedContsT[W, M, B, O, A0] = fa.imap(f)
}

private[scalaz] sealed trait IndexedContsTFunctorRight[W[_], M[_], R, O] extends Functor[({type f[a]=IndexedContsT[W, M, R, O, a]})#f] {
  implicit val W: Functor[W]

  override def map[A, B](fa: IndexedContsT[W, M, R, O, A])(f: A => B): IndexedContsT[W, M, R, O, B] = fa.map(f)
}

private[scalaz] sealed trait IndexedContsTContravariant[W[_], M[_], R, A0] extends Contravariant[({type f[o]=IndexedContsT[W, M, R, o, A0]})#f] {
  implicit val W: Functor[W]
  implicit val M: Functor[M]

  def contramap[A, B](fa: IndexedContsT[W, M, R, A, A0])(f: B => A): IndexedContsT[W, M, R, B, A0] = fa.contramap(f)
}

private[scalaz] sealed trait IndexedContsTBifunctor[W[_], M[_], O] extends Bifunctor[({type f[r, a]=IndexedContsT[W, M, r, O, a]})#f] {
  implicit val W: Functor[W]
  implicit val M: Functor[M]

  def bimap[A, B, C, D](fab: IndexedContsT[W, M, A, O, B])(f: A => C, g: B => D): IndexedContsT[W, M, C, O, D] = fab.bimap(f, g)

  override def leftFunctor[X]: Functor[({type f[r]=IndexedContsT[W, M, r, O, X]})#f] = IndexedContsT.IndexedContsTFunctorLeft

  override def leftMap[A, B, C](fa: IndexedContsT[W, M, A, O, B])(f: A => C): IndexedContsT[W, M, C, O, B] = fa.imap(f)

  override def rightFunctor[X]: Functor[({type f[a]=IndexedContsT[W, M, X, O, a]})#f] = IndexedContsT.IndexedContsTFunctorRight

  override def rightMap[A, B, D](fa: IndexedContsT[W, M, A, O, B])(f: B => D): IndexedContsT[W, M, A, O, D] = fa.map(f)
}

private[scalaz] sealed trait ContsTMonad[W[_], M[_], R] extends Monad[({type f[a]=ContsT[W, M, R, a]})#f] with IndexedContsTFunctorRight[W, M, R, R] {
  implicit val W: Comonad[W]

  def point[A](a: => A): ContsT[W, M, R, A] = IndexedContsT.point(a)

  def bind[A, B](fa: ContsT[W, M, R, A])(f: A => ContsT[W, M, R, B]): ContsT[W, M, R, B] = fa.flatMap(f)

  override def join[A](ffa: ContsT[W, M, R, ContsT[W, M, R, A]]): ContsT[W, M, R, A] = ffa.flatten
}
