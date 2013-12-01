package scalaz

import Id._

final case class WriterT[F[_], W, A](run: F[(W, A)]) { self =>
  import WriterT._

  def off: UnwriterT[F, W, A] =
    UnwriterT(run)

  /** alias for `off` */
  def unary_- : UnwriterT[F, W, A] =
    UnwriterT(run)

  def mapValue[X, B](f: ((W, A)) => (X, B))(implicit F: Functor[F]): WriterT[F, X, B] =
    writerT(F.map(run)(f))

  def mapWritten[X](f: W => X)(implicit F: Functor[F]): WriterT[F, X, A] =
    mapValue(wa => (f(wa._1), wa._2))

  def written(implicit F: Functor[F]): F[W] =
    F.map(run)(_._1)

  def value(implicit F: Functor[F]): F[A] =
    F.map(run)(_._2)

  def swap(implicit F: Functor[F]): WriterT[F, A, W] =
    mapValue(wa => (wa._2, wa._1))

  def :++>(w: => W)(implicit F: Functor[F], W: Semigroup[W]): WriterT[F, W, A] =
    mapWritten(W.append(_, w))

  def :++>>(f: A => W)(implicit F: Functor[F], W: Semigroup[W]): WriterT[F, W, A] =
    mapValue(wa => (W.append(wa._1, f(wa._2)), wa._2))

  def <++:(w: => W)(implicit F: Functor[F], W: Semigroup[W]): WriterT[F, W, A] =
    mapWritten(W.append(w, _))

  def <<++:(f: A => W)(implicit F: Functor[F], s: Semigroup[W]): WriterT[F, W, A] =
    mapValue(wa => (s.append(f(wa._2), wa._1), wa._2))

  def reset(implicit Z: Monoid[W], F: Functor[F]): WriterT[F, W, A] =
    mapWritten(_ => Z.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): WriterT[F, W, B] =
    writerT(F.map(run)(wa => (wa._1, f(wa._2))))

  @deprecated("Each/foreach is deprecated", "7.1")
  def foreach[B](f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(wa => f(wa._2))

  def ap[B](f: => WriterT[F, W, A => B])(implicit F: Apply[F], W: Semigroup[W]): WriterT[F, W, B] = writerT {
    F.apply2(f.run, run) {
      case ((w1, fab), (w2, a)) => (W.append(w1, w2), fab(a))
    }
  }

  def flatMap[B](f: A => WriterT[F, W, B])(implicit F: Bind[F], s: Semigroup[W]): WriterT[F, W, B] =
    writerT(F.bind(run){wa =>
      val z = f(wa._2).run
      F.map(z)(wb => (s.append(wa._1, wb._1), wb._2))
    })

  def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[WriterT[F, W, B]] = {
    G.map(F.traverse(run){
      case (w, a) => G.map(f(a))(b => (w, b))
    })(WriterT(_))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]) =
    F.foldr(run, z) { a => b =>
      f(a._2, b)
    }

  def bimap[C, D](f: W => C, g: A => D)(implicit F: Functor[F]) =
    writerT[F, C, D](F.map(run)({
      case (a, b) => (f(a), g(b))
    }))

  def leftMap[C](f: W => C)(implicit F: Functor[F]): WriterT[F, C, A] =
    bimap(f, identity)

  def bitraverse[G[_], C, D](f: W => G[C], g: A => G[D])(implicit G: Applicative[G], F: Traverse[F]) =
    G.map(F.traverse[G, (W, A), (C, D)](run) {
      case (a, b) => G.apply2(f(a), g(b))((_, _))
    })(writerT(_))

  def rwst[R, S](implicit F: Functor[F]): ReaderWriterStateT[F, R, W, S, A] = ReaderWriterStateT(
    (r, s) => F.map(self.run) {
      case (w, a) => (w, a, s)
    }
  )

  def wpoint[G[_]](implicit F: Functor[F], P: Applicative[G]): WriterT[F, G[W], A] =
    writerT(F.map(self.run) {
      case (w, a) => (P.point(w), a)
    })

  def colocal[X](f: W => X)(implicit F: Functor[F]): WriterT[F, X, A] = mapWritten(f)
}

object WriterT extends WriterTInstances with WriterTFunctions

sealed abstract class WriterTInstances12 {
  implicit def writerFunctor[W]: Functor[({type λ[α]=Writer[W, α]})#λ] = new WriterTFunctor[Id, W] {
    implicit def F = idInstance
  }
}
sealed abstract class WriterTInstances11 extends WriterTInstances12 {
  implicit def writerTFunctor[F[_], W](implicit F0: Functor[F]): Functor[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTFunctor[F, W] {
    implicit def F = F0
  }
}

sealed abstract class WriterTInstances10 extends WriterTInstances11 {
  implicit def writerApply[W](implicit W0: Semigroup[W]): Apply[({type λ[α]=Writer[W, α]})#λ] = new WriterTApply[Id, W] {
    implicit def F = idInstance
    implicit def W = W0
  }
}

sealed abstract class WriterTInstances9 extends WriterTInstances10 {
  implicit def writerTApply[F[_], W](implicit W0: Semigroup[W], F0: Apply[F]): Apply[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTApply[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

sealed abstract class WriterTInstances8 extends WriterTInstances9 {
  implicit def writerApplicative[W](implicit W0: Monoid[W]): Applicative[({type λ[α]=Writer[W, α]})#λ] = new WriterTApplicative[Id, W] {
    implicit def F = idInstance
    implicit def W = W0
  }
}

sealed abstract class WriterTInstances7 extends WriterTInstances8 {
  implicit def writerTApplicative[F[_], W](implicit W0: Monoid[W], F0: Applicative[F]): Applicative[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTApplicative[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

sealed abstract class WriterTInstances6 extends WriterTInstances7 {
  implicit def writerMonad[W](implicit W0: Monoid[W]): Monad[({type λ[α]=Writer[W, α]})#λ] = new WriterTMonad[Id, W] {
    implicit def F = idInstance
    implicit def W = W0
  }
}

sealed abstract class WriterTInstance5 extends WriterTInstances6 {
  implicit def writerTMonad[F[_], W](implicit W0: Monoid[W], F0: Monad[F]): Monad[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTMonad[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

sealed abstract class WriterTInstances4 extends WriterTInstance5 {
  implicit def writerFoldable[W]: Foldable[({type λ[α]=Writer[W, α]})#λ] = new WriterTFoldable[Id, W] {
    implicit def F = idInstance
  }
  implicit def writerEqual[W, A](implicit E: Equal[(W, A)]): Equal[Writer[W, A]] = E.contramap((_: Writer[W, A]).run)
}

sealed abstract class WriterTInstances3 extends WriterTInstances4 {
  implicit def writerTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[({type λ[α, β]=WriterT[F, α, β]})#λ] = new WriterTBifunctor[F] {
    implicit def F = F0
  }
  implicit def writerTFoldable[F[_], W](implicit F0: Foldable[F]): Foldable[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTFoldable[F, W] {
    implicit def F = F0
  }
  implicit def writerTEqual[F[_], W, A](implicit E: Equal[F[(W, A)]]): Equal[WriterT[F, W, A]] = E.contramap((_: WriterT[F, W, A]).run)
}

sealed abstract class WriterTInstances2 extends WriterTInstances3 {
  implicit def writerComonad[W]: Comonad[({type λ[α]=Writer[W, α]})#λ] = new WriterComonad[W] {
    implicit def F = implicitly
  }
}

sealed abstract class WriterTInstances1 extends WriterTInstances2 {
  implicit val writerBitraverse: Bitraverse[Writer] = new WriterTBitraverse[Id] {
    implicit def F = idInstance
  }
  implicit def writerTraverse[W]: Traverse[({type λ[α]=Writer[W, α]})#λ] = new WriterTTraverse[Id, W] {
    implicit def F = idInstance
  }
  @deprecated("Each/foreach is deprecated", "7.1")
  implicit def writerEach[W]: Each[({type λ[α]=Writer[W, α]})#λ] = new WriterTEach[Id, W] {
    implicit def F = idInstance
  }
}

sealed abstract class WriterTInstances0 extends WriterTInstances1 {
  implicit def writerTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[({type λ[α, β]=WriterT[F, α, β]})#λ] = new WriterTBitraverse[F] {
    implicit def F = F0
  }
  implicit def writerTTraverse[F[_], W](implicit F0: Traverse[F]): Traverse[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTTraverse[F, W] {
    implicit def F = F0
  }
  @deprecated("Index is deprecated, use Foldable instead", "7.1")
  implicit def writerIndex[W]: Index[({type λ[α]=Writer[W, α]})#λ] = new WriterIndex[W] {
  }
  @deprecated("Each/foreach is deprecated", "7.1")
  implicit def writerTEach[F[_], W](implicit F0: Each[F]): Each[({type λ[α]=WriterT[F, W, α]})#λ] = new WriterTEach[F, W] {
    implicit def F = F0
  }
}

sealed abstract class WriterTInstances extends WriterTInstances0 {
  implicit def writerTMonadListen[F[_], W](implicit F0: Monad[F], W0: Monoid[W]): MonadListen[({type λ[α, β] = WriterT[F, α, β]})#λ, W] = new WriterTMonadListen[F, W] {
    implicit def F = F0
    implicit def W = W0
  }

  implicit def writerTHoist[W](implicit W0: Monoid[W]): Hoist[({type λ[α[_], β] = WriterT[α, W, β]})#λ] = new WriterTHoist[W] {
    implicit def W = W0
  }
}

trait WriterTFunctions {
  def writerT[F[_], W, A](v: F[(W, A)]): WriterT[F, W, A] = WriterT(v)

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT[Id, W, A](v)

  def tell[W](w: W): Writer[W, Unit] = writer(w -> ())

  def put[F[_], W, A](value: F[A])(w: W)(implicit F: Functor[F]): WriterT[F, W, A] =
    WriterT(F.map(value)(a => (w, a)))

  /** Puts the written value that is produced by applying the given function into a writer transformer and associates with `value` */
  def putWith[F[_], W, A](value: F[A])(w: A => W)(implicit F: Functor[F]): WriterT[F, W, A] =
    WriterT(F.map(value)(a => (w(a), a)))

}

//
// Type class implementation traits
//
import WriterT.writerT

private trait WriterTFunctor[F[_], W] extends Functor[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WriterT[F, W, A])(f: A => B) = fa map f
}

private trait WriterTApply[F[_], W] extends Apply[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTFunctor[F, W] {
  implicit def F: Apply[F]
  implicit def W: Semigroup[W]

  override def ap[A, B](fa: => WriterT[F, W, A])(f: => WriterT[F, W, A => B]) = fa ap f
}

private trait WriterTApplicative[F[_], W] extends Applicative[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTApply[F, W] {
  implicit def F: Applicative[F]
  implicit def W: Monoid[W]
  def point[A](a: => A) = writerT(F.point((W.zero, a)))
}

private trait WriterTEach[F[_], W] extends Each[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def F: Each[F]

  def each[A](fa: WriterT[F, W, A])(f: A => Unit) = fa foreach f
}

// TODO does Index it make sense for F other than Id?
private trait WriterIndex[W] extends Index[({type λ[α]=Writer[W, α]})#λ] {
  def index[A](fa: Writer[W, A], i: Int) = if(i == 0) Some(fa.value) else None
}

private trait WriterTMonad[F[_], W] extends Monad[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTApplicative[F, W] {
  implicit def F: Monad[F]

  def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) = fa flatMap f
}

private trait WriterTFoldable[F[_], W] extends Foldable.FromFoldr[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: WriterT[F, W, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
}

private trait WriterTTraverse[F[_], W] extends Traverse[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTFoldable[F, W] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: WriterT[F, W, A])(f: A => G[B]) = fa traverse f
}

private trait WriterTBifunctor[F[_]] extends Bifunctor[({type λ[α, β]=WriterT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D) =
    fab.bimap(f, g)
}

private trait WriterTBitraverse[F[_]] extends Bitraverse[({type λ[α, β]=WriterT[F, α, β]})#λ] with WriterTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: WriterT[F, A, B])(f: A => G[C], g: B => G[D]) =
    fab.bitraverse(f, g)
}

private trait WriterComonad[W] extends Comonad[({type λ[α] = Writer[W, α]})#λ] with WriterTFunctor[Id, W] {
  def copoint[A](p: Writer[W, A]): A = p.value

  override def cojoin[A](fa: Writer[W, A]): Writer[W, Writer[W, A]] =
    Writer(fa.written, fa)

  override def cobind[A, B](fa: Writer[W, A])(f: (Writer[W, A]) => B): Writer[W, B] =
    Writer(fa.written, f(fa))
}

private trait WriterTHoist[W] extends Hoist[({type λ[α[_], β] = WriterT[α, W, β]})#λ] {
  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): WriterT[M, W, B] =
    WriterT(M.map(mb)((W.zero, _)))

  implicit def W: Monoid[W]

  implicit def apply[M[_]: Monad]: Monad[({type λ[α]=WriterT[M, W, α]})#λ] = WriterT.writerTMonad

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type λ[α]=WriterT[M, W, α]})#λ ~> ({type λ[α]=WriterT[N, W, α]})#λ) {
    def apply[A](fa: WriterT[M, W, A]) = WriterT(f(fa.run))
  }
}

private trait WriterTMonadListen[F[_], W] extends MonadListen[({type λ[α, β] = WriterT[F, α, β]})#λ, W] with WriterTMonad[F, W] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def writer[A](w: W, v: A): WriterT[F, W, A] = WriterT.writerT(F.point((w, v)))

  def listen[A](fa: WriterT[F, W, A]): WriterT[F, W, (A, W)] =
    WriterT(F.bind(fa.run){ case (w, a) => F.point((w, (a, w))) })
}
