package scalaz

import Id._

sealed trait WriterT[F[+_], +W, +A] { self =>
  val run: F[(W, A)]

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

  def :++>[WW >: W](w: => WW)(implicit F: Functor[F], W: Semigroup[WW]): WriterT[F, WW, A] =
    mapWritten(W.append(_, w))

  def :++>>[WW >: W](f: A => WW)(implicit F: Functor[F], W: Semigroup[WW]): WriterT[F, WW, A] =
    mapValue(wa => (W.append(wa._1, f(wa._2)), wa._2))

  def <++:[WW >: W](w: => WW)(implicit F: Functor[F], W: Semigroup[WW]): WriterT[F, WW, A] =
    mapWritten(W.append(w, _))

  def <<++:[WW >: W](f: A => WW)(implicit F: Functor[F], s: Semigroup[WW]): WriterT[F, WW, A] =
    mapValue(wa => (s.append(f(wa._2), wa._1), wa._2))

  def reset[WW >: W](implicit Z: Monoid[WW], F: Functor[F]): WriterT[F, WW, A] =
    mapWritten(_ => Z.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): WriterT[F, W, B] =
    writerT(F.map(run)(wa => (wa._1, f(wa._2))))

  def foreach[B](f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(wa => f(wa._2))

  def ap[B, WW >: W](f: => WriterT[F, WW, A => B])(implicit F: Apply[F], W: Semigroup[WW]): WriterT[F, WW, B] = writerT {
    F.apply2(f.run, run) {
      case ((w1, fab), (w2, a)) => (W.append(w1, w2), fab(a))
    }
  }

  def flatMap[B, WW >: W](f: A => WriterT[F, WW, B])(implicit F: Bind[F], s: Semigroup[WW]): WriterT[F, WW, B] =
    writerT(F.bind(run){wa =>
      val z = f(wa._2).run
      F.map(z)(wb => (s.append(wa._1, wb._1), wb._2))
    })

  def traverse[G[_] , WW >: W, B](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[WriterT[F, WW, B]] = {
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

  def wpoint[G[+_]](implicit F: Functor[F], P: Applicative[G]): WriterT[F, G[W], A] =
    writerT(F.map(self.run) {
      case (w, a) => (P.point(w), a)
    })

  def colocal[X](f: W => X)(implicit F: Functor[F]): WriterT[F, X, A] = mapWritten(f)
}

object WriterT extends WriterTFunctions with WriterTInstances {
  def apply[F[+_], W, A](v: F[(W, A)]): WriterT[F, W, A] =
    writerT(v)
}

trait WriterTInstances12 {
  implicit def writerFunctor[W]: WriterTFunctor[Id, W] = new WriterTFunctor[Id, W] {
    implicit def F = idInstance
  }
}
trait WriterTInstances11 extends WriterTInstances12 {
  implicit def writerTFunctor[F[+_], W](implicit F0: Functor[F]) = new WriterTFunctor[F, W] {
    implicit def F = F0
  }
}

trait WriterTInstances10 extends WriterTInstances11 {
  implicit def writerApply[W](implicit W0: Semigroup[W]) = new WriterTApply[Id, W] {
    implicit def F = idInstance
    implicit def W = W0
  }
}

trait WriterTInstances9 extends WriterTInstances10 {
  implicit def writerTApply[F[+_], W](implicit W0: Semigroup[W], F0: Apply[F]) = new WriterTApply[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

trait WriterTInstances8 extends WriterTInstances9 {
  implicit def writerApplicative[W](implicit W0: Monoid[W]) = new WriterTApplicative[Id, W] {
    implicit def F = idInstance
    implicit def W = W0
  }
}

trait WriterTInstances7 extends WriterTInstances8 {
  implicit def writerTApplicative[F[+_], W](implicit W0: Monoid[W], F0: Applicative[F]) = new WriterTApplicative[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

trait WriterTInstances6 extends WriterTInstances7 {
  implicit def writerMonad[W](implicit W0: Monoid[W]) = new WriterTMonad[Id, W] {
    implicit def F = idInstance
    implicit def W = W0
  }
}

trait WriterTInstance5 extends WriterTInstances6 {
  implicit def writerTMonad[F[+_], W](implicit W0: Monoid[W], F0: Monad[F]) = new WriterTMonad[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

trait WriterTInstances4 extends WriterTInstance5 {
  implicit def writerBifunctor = new WriterTBifunctor[Id] {
    implicit def F = idInstance
  }
  implicit def writerFoldable[W] = new WriterTFoldable[Id, W] {
    implicit def F = idInstance
  }
  implicit def writerEqual[W, A](implicit E: Equal[(W, A)]) = E.contramap((_: Writer[W, A]).run)
}

trait WriterTInstances3 extends WriterTInstances4 {
  implicit def writerTBifunctor[F[+_]](implicit F0: Functor[F]) = new WriterTBifunctor[F] {
    implicit def F = F0
  }
  implicit def writerTFoldable[F[+_], W](implicit F0: Foldable[F]) = new WriterTFoldable[F, W] {
    implicit def F = F0
  }
  implicit def writerTEqual[F[+_], W, A](implicit E: Equal[F[(W, A)]]) = E.contramap((_: WriterT[F, W, A]).run)
}

trait WriterTInstances2 extends WriterTInstances3 {
  implicit def writerComonad[W] = new WriterComonad[W] {
    implicit def F = implicitly
  }
}

trait WriterTInstances1 extends WriterTInstances2 {
  implicit def writerBitraverse: WriterTBitraverse[Id] = new WriterTBitraverse[Id] {
    implicit def F = idInstance
  }
  implicit def writerTraverse[W]: WriterTTraverse[Id, W] = new WriterTTraverse[Id, W] {
    implicit def F = idInstance
  }
  implicit def writerEach[W]: WriterTEach[Id, W] = new WriterTEach[Id, W] {
    implicit def F = idInstance
  }
}

trait WriterTInstances0 extends WriterTInstances1 {
  implicit def writerTBitraverse[F[+_]](implicit F0: Traverse[F]) = new WriterTBitraverse[F] {
    implicit def F = F0
  }
  implicit def writerTTraverse[F[+_], W](implicit F0: Traverse[F]) = new WriterTTraverse[F, W] {
    implicit def F = F0
  }
  implicit def writerTIndex[W] = new WriterTIndex[W] {
  }
  implicit def writerEach[F[+_], W](implicit F0: Each[F]) = new WriterTEach[F, W] {
    implicit def F = F0
  }
}

trait WriterTInstances extends WriterTInstances0 {
  implicit def writerTMonadListen[F[+_], W](implicit F0: Monad[F], W0: Monoid[W]) = new WriterMonadListen[F, W] {
    implicit def F = F0
    implicit def W = W0
  }

  implicit def writerTMonadTrans[W](implicit W0: Monoid[W]): MonadTrans[({type λ[α[+_], β] = WriterT[α, W, β]})#λ] = new WriterTMonadTrans[W] {
    implicit def W = W0
  }
}

trait WriterTFunctions {
  def writerT[F[+_], W, A](v: F[(W, A)]): WriterT[F, W, A] = new WriterT[F, W, A] {
    val run = v
  }

  import StoreT._

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT[Id, W, A](v)

  def tell[W](w: W): Writer[W, Unit] = writer(w -> ())

  def put[F[+_], W, A](value: F[A])(w: W)(implicit F: Functor[F]): WriterT[F, W, A] =
    WriterT(F.map(value)(a => (w, a)))

  /** Puts the written value that is produced by applying the given function into a writer transformer and associates with `value` */
  def putWith[F[+_], W, A](value: F[A])(w: A => W)(implicit F: Functor[F]): WriterT[F, W, A] =
    WriterT(F.map(value)(a => (w(a), a)))

}

//
// Type class implementation traits
//
import WriterT.writerT

private[scalaz] trait WriterTFunctor[F[+_], W] extends Functor[({type λ[+α]=WriterT[F, W, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WriterT[F, W, A])(f: A => B) = fa map f
}

private[scalaz] trait WriterTApply[F[+_], W] extends Apply[({type λ[+α]=WriterT[F, W, α]})#λ] with WriterTFunctor[F, W] {
  implicit def F: Apply[F]
  implicit def W: Semigroup[W]

  override def ap[A, B](fa: => WriterT[F, W, A])(f: => WriterT[F, W, A => B]) = fa ap f
}

private[scalaz] trait WriterTApplicative[F[+_], W] extends Applicative[({type λ[+α]=WriterT[F, W, α]})#λ] with WriterTApply[F, W] {
  implicit def F: Applicative[F]
  implicit def W: Monoid[W]
  def point[A](a: => A) = writerT(F.point((W.zero, a)))
}

private[scalaz] trait WriterTEach[F[+_], W] extends Each[({type λ[+α]=WriterT[F, W, α]})#λ] {
  implicit def F: Each[F]

  def each[A](fa: WriterT[F, W, A])(f: A => Unit) = fa foreach f
}

// TODO does Index it make sense for F other than Id?
private[scalaz] trait WriterTIndex[W] extends Index[({type λ[+α]=WriterT[Id, W, α]})#λ] {
  def index[A](fa: WriterT[Id, W, A], i: Int) = if(i == 0) Some(fa.value) else None
}

private[scalaz] trait WriterTMonad[F[+_], W] extends Monad[({type λ[+α]=WriterT[F, W, α]})#λ] with WriterTApplicative[F, W] {
  implicit def F: Monad[F]

  def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) = fa flatMap f
}

private[scalaz] trait WriterTFoldable[F[+_], W] extends Foldable.FromFoldr[({type λ[+α]=WriterT[F, W, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: WriterT[F, W, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
}

private[scalaz] trait WriterTTraverse[F[+_], W] extends Traverse[({type λ[+α]=WriterT[F, W, α]})#λ] with WriterTFoldable[F, W] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: WriterT[F, W, A])(f: A => G[B]) = fa traverse f
}

private[scalaz] trait WriterTBifunctor[F[+_]] extends Bifunctor[({type λ[+α, +β]=WriterT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D) =
    fab.bimap(f, g)
}

private[scalaz] trait WriterTBitraverse[F[+_]] extends Bitraverse[({type λ[+α, +β]=WriterT[F, α, β]})#λ] with WriterTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: WriterT[F, A, B])(f: A => G[C], g: B => G[D]) =
    fab.bitraverse(f, g)
}

private[scalaz] trait WriterComonad[W] extends Comonad[({type λ[+α] = Writer[W, α]})#λ] with WriterTFunctor[Id, W] {
  def copoint[A](p: Writer[W, A]): A = p.value

  override def cojoin[A](fa: Writer[W, A]): Writer[W, Writer[W, A]] =
    Writer(fa.written, fa)

  override def cobind[A, B](fa: Writer[W, A])(f: (Writer[W, A]) => B): Writer[W, B] =
    Writer(fa.written, f(fa))
}

private[scalaz] trait WriterTMonadTrans[W] extends MonadTrans[({type λ[α[+_], β] = WriterT[α, W, β]})#λ] {
  def liftM[M[+_], B](mb: M[B])(implicit M: Monad[M]): WriterT[M, W, B] =
    WriterT(M.map(mb)((W.zero, _)))

  implicit def W: Monoid[W]

  implicit def apply[M[+_]: Monad]: Monad[({type λ[α]=WriterT[M, W, α]})#λ] = WriterT.writerTMonad
}

private[scalaz] trait WriterMonadListen[F[+_], W] extends MonadListen[({type λ[+α, +β] = WriterT[F, α, β]})#λ, W] with WriterTMonad[F, W] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def writer[A](w: W, v: A): WriterT[F, W, A] = WriterT.writerT(F.point((w, v)))

  def listen[A](fa: WriterT[F, W, A]): WriterT[F, W, (A, W)] =
    WriterT(F.bind(fa.run){ case (w, a) => F.point((w, (a, w))) })
}
