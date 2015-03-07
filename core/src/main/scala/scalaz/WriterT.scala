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

  def ap[B](f: => WriterT[F, W, A => B])(implicit F: Apply[F], W: Semigroup[W]): WriterT[F, W, B] = writerT {
    F.apply2(f.run, run) {
      case ((w1, fab), (w2, a)) => (W.append(w1, w2), fab(a))
    }
  }

  def flatMap[B](f: A => WriterT[F, W, B])(implicit F: Bind[F], s: Semigroup[W]): WriterT[F, W, B] =
    flatMapF(f.andThen(_.run))

  def flatMapF[B](f: A => F[(W, B)])(implicit F: Bind[F], s: Semigroup[W]): WriterT[F, W, B] =
    writerT(F.bind(run){wa =>
      val z = f(wa._2)
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
      case (a, b) => G.tuple2(f(a), g(b))
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

sealed abstract class WriterTInstances13 {
  implicit def writerTMonoid[F[_], W, A](implicit M: Monoid[F[(W,A)]]): Monoid[WriterT[F, W, A]] =
    new Monoid[WriterT[F, W, A]] {
      def zero = WriterT(M.zero)
      def append(a: WriterT[F, W, A], b: => WriterT[F, W, A]) = WriterT(M.append(a.run, b.run))
    }
}

sealed abstract class WriterTInstances12 extends WriterTInstances13 {
  implicit def writerFunctor[W]: Functor[Writer[W, ?]] =
    new WriterTFunctor[Id, W] {
      implicit def F = idInstance
    }
}
sealed abstract class WriterTInstances11 extends WriterTInstances12 {
  implicit def writerTFunctor[F[_], W](implicit F0: Functor[F]): Functor[WriterT[F, W, ?]] =
    new WriterTFunctor[F, W] {
      implicit def F = F0
    }
}

sealed abstract class WriterTInstances10 extends WriterTInstances11 {
  implicit def writerApply[W](implicit W0: Semigroup[W]): Apply[Writer[W, ?]] =
    new WriterTApply[Id, W] {
      implicit def F = idInstance
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances9 extends WriterTInstances10 {
  implicit def writerTApply[F[_], W](implicit W0: Semigroup[W], F0: Apply[F]): Apply[WriterT[F, W, ?]] =
    new WriterTApply[F, W] {
      implicit def F = F0
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances8 extends WriterTInstances9 {
  implicit def writerApplicative[W](implicit W0: Monoid[W]): Applicative[Writer[W, ?]] =
    new WriterTApplicative[Id, W] {
      implicit def F = idInstance
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances7 extends WriterTInstances8 {
  implicit def writerTApplicative[F[_], W](implicit W0: Monoid[W], F0: Applicative[F]): Applicative[WriterT[F, W, ?]] =
    new WriterTApplicative[F, W] {
      implicit def F = F0
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances6 extends WriterTInstances7 {
  implicit def writerMonad[W](implicit W0: Monoid[W]): Monad[Writer[W, ?]] =
    new WriterTMonad[Id, W] {
      implicit def F = idInstance
      implicit def W = W0
    }
}

sealed abstract class WriterTInstance5 extends WriterTInstances6 {
  implicit def writerTMonad[F[_], W](implicit W0: Monoid[W], F0: Monad[F]): Monad[WriterT[F, W, ?]] =
    new WriterTMonad[F, W] {
      implicit def F = F0
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances4 extends WriterTInstance5 {
  implicit def writerFoldable[W]: Foldable[Writer[W, ?]] =
    new WriterTFoldable[Id, W] {
      implicit def F = idInstance
    }
  implicit def writerEqual[W, A](implicit E: Equal[(W, A)]): Equal[Writer[W, A]] = E.contramap((_: Writer[W, A]).run)
}

sealed abstract class WriterTInstances3 extends WriterTInstances4 {
  implicit def writerTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[WriterT[F, ?, ?]] =
    new WriterTBifunctor[F] {
      implicit def F = F0
    }
  implicit def writerTFoldable[F[_], W](implicit F0: Foldable[F]): Foldable[WriterT[F, W, ?]] =
    new WriterTFoldable[F, W] {
      implicit def F = F0
    }
  implicit def writerTEqual[F[_], W, A](implicit E: Equal[F[(W, A)]]): Equal[WriterT[F, W, A]] = E.contramap((_: WriterT[F, W, A]).run)
}

sealed abstract class WriterTInstances2 extends WriterTInstances3 {
  implicit def writerComonad[W]: Comonad[Writer[W, ?]] = 
    new WriterComonad[W] {
      implicit def F = implicitly
    }
}

sealed abstract class WriterTInstances1 extends WriterTInstances2 {
  implicit val writerBitraverse: Bitraverse[Writer] =
    new WriterTBitraverse[Id] {
      implicit def F = idInstance
    }
  implicit def writerTraverse[W]: Traverse[Writer[W, ?]] = 
    new WriterTTraverse[Id, W] {
      implicit def F = idInstance
    }
}

sealed abstract class WriterTInstances0 extends WriterTInstances1 {
  implicit def writerTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[WriterT[F, ?, ?]] =
    new WriterTBitraverse[F] {
      implicit def F = F0
    }
  implicit def writerTTraverse[F[_], W](implicit F0: Traverse[F]): Traverse[WriterT[F, W, ?]] = 
    new WriterTTraverse[F, W] {
      implicit def F = F0
    }
}

sealed abstract class WriterTInstances extends WriterTInstances0 {
  implicit def writerTMonadListen[F[_], W](implicit F0: Monad[F], W0: Monoid[W]): MonadListen[WriterT[F, ?, ?], W] =
    new WriterTMonadListen[F, W] {
      implicit def F = F0
      implicit def W = W0
    }

  implicit def writerTHoist[W](implicit W0: Monoid[W]): Hoist[λ[(α[_], β) => WriterT[α, W, β]]] =
    new WriterTHoist[W] {
      implicit def W = W0
    }

  implicit def writerTShow[F[_], W, A](implicit F0: Show[F[(W, A)]]): Show[WriterT[F, W, A]] =
    Contravariant[Show].contramap(F0)(_.run)
}

trait WriterTFunctions {
  def writerT[F[_], W, A](v: F[(W, A)]): WriterT[F, W, A] = WriterT(v)

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT[Id, W, A](v)

  def tell[W](w: W): Writer[W, Unit] = writer((w, ()))

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

private trait WriterTFunctor[F[_], W] extends Functor[WriterT[F, W, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WriterT[F, W, A])(f: A => B) = fa map f
}

private trait WriterTApply[F[_], W] extends Apply[WriterT[F, W, ?]] with WriterTFunctor[F, W] {
  implicit def F: Apply[F]
  implicit def W: Semigroup[W]

  override def ap[A, B](fa: => WriterT[F, W, A])(f: => WriterT[F, W, A => B]) = fa ap f
}

private trait WriterTApplicative[F[_], W] extends Applicative[WriterT[F, W, ?]] with WriterTApply[F, W] {
  implicit def F: Applicative[F]
  implicit def W: Monoid[W]
  def point[A](a: => A) = writerT(F.point((W.zero, a)))
}


private trait WriterTMonad[F[_], W] extends Monad[WriterT[F, W, ?]] with WriterTApplicative[F, W] {
  implicit def F: Monad[F]

  def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]) = fa flatMap f
}

private trait WriterTFoldable[F[_], W] extends Foldable.FromFoldr[WriterT[F, W, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: WriterT[F, W, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
}

private trait WriterTTraverse[F[_], W] extends Traverse[WriterT[F, W, ?]] with WriterTFoldable[F, W] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: WriterT[F, W, A])(f: A => G[B]) = fa traverse f
}

private trait WriterTBifunctor[F[_]] extends Bifunctor[WriterT[F, ?, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: A => C, g: B => D) =
    fab.bimap(f, g)
}

private trait WriterTBitraverse[F[_]] extends Bitraverse[WriterT[F, ?, ?]] with WriterTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: WriterT[F, A, B])(f: A => G[C], g: B => G[D]) =
    fab.bitraverse(f, g)
}

private trait WriterComonad[W] extends Comonad[Writer[W, ?]] with WriterTFunctor[Id, W] {
  def copoint[A](p: Writer[W, A]): A = p.value

  override def cojoin[A](fa: Writer[W, A]): Writer[W, Writer[W, A]] =
    Writer(fa.written, fa)

  override def cobind[A, B](fa: Writer[W, A])(f: (Writer[W, A]) => B): Writer[W, B] =
    Writer(fa.written, f(fa))
}

private trait WriterTHoist[W] extends Hoist[λ[(α[_], β) => WriterT[α, W, β]]] {
  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): WriterT[M, W, B] =
    WriterT(M.map(mb)((W.zero, _)))

  implicit def W: Monoid[W]

  implicit def apply[M[_]: Monad]: Monad[WriterT[M, W, ?]] = WriterT.writerTMonad

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    new (WriterT[M, W, ?] ~> WriterT[N, W, ?]) {
      def apply[A](fa: WriterT[M, W, A]) = WriterT(f(fa.run))
    }
}

private trait WriterTMonadListen[F[_], W] extends MonadListen[WriterT[F, ?, ?], W] with WriterTMonad[F, W] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def writer[A](w: W, v: A): WriterT[F, W, A] = WriterT.writerT(F.point((w, v)))

  def listen[A](fa: WriterT[F, W, A]): WriterT[F, W, (A, W)] =
    WriterT(F.bind(fa.run){ case (w, a) => F.point((w, (a, w))) })
}
