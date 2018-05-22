package scalaz

import Id._

final case class WriterT[W, F[_], A](run: F[(W, A)]) { self =>
  import WriterT._

  def off: UnwriterT[F, W, A] =
    UnwriterT(run)

  /** alias for `off` */
  def unary_- : UnwriterT[F, W, A] =
    UnwriterT(run)

  def mapValue[X, B](f: ((W, A)) => (X, B))(implicit F: Functor[F]): WriterT[X, F, B] =
    writerT(F.map(run)(f))

  def mapWritten[X](f: W => X)(implicit F: Functor[F]): WriterT[X, F, A] =
    mapValue(wa => (f(wa._1), wa._2))

  def mapT[W2, G[_], B](f: F[(W, A)] => G[(W2, B)]): WriterT[W2, G, B] =
    WriterT(f(run))

  def written(implicit F: Functor[F]): F[W] =
    F.map(run)(_._1)

  def value(implicit F: Functor[F]): F[A] =
    F.map(run)(_._2)

  def swap(implicit F: Functor[F]): WriterT[A, F, W] =
    mapValue(wa => (wa._2, wa._1))

  def :++>(w: => W)(implicit F: Functor[F], W: Semigroup[W]): WriterT[W, F, A] =
    mapWritten(W.append(_, w))

  def :++>>(f: A => W)(implicit F: Functor[F], W: Semigroup[W]): WriterT[W, F, A] =
    mapValue(wa => (W.append(wa._1, f(wa._2)), wa._2))

  def <++:(w: W)(implicit F: Functor[F], W: Semigroup[W]): WriterT[W, F, A] =
    mapWritten(W.append(w, _))

  def <<++:(f: A => W)(implicit F: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapValue(wa => (s.append(f(wa._2), wa._1), wa._2))

  def reset(implicit Z: Monoid[W], F: Functor[F]): WriterT[W, F, A] =
    mapWritten(_ => Z.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): WriterT[W, F, B] =
    writerT(F.map(run)(wa => (wa._1, f(wa._2))))

  def mapF[B](f: A => F[B])(implicit F: Bind[F]): WriterT[W, F, B] = writerT {
    F.bind(run)(wa => F.map(f(wa._2))(a => (wa._1, a)))
  }

  def ap[B](f: => WriterT[W, F, A => B])(implicit F: Apply[F], W: Semigroup[W]): WriterT[W, F, B] = writerT {
    F.apply2(f.run, run) {
      case ((w1, fab), (w2, a)) => (W.append(w1, w2), fab(a))
    }
  }

  def flatMap[B](f: A => WriterT[W, F, B])(implicit F: Bind[F], s: Semigroup[W]): WriterT[W, F, B] =
    flatMapF(f.andThen(_.run))

  def flatMapF[B](f: A => F[(W, B)])(implicit F: Bind[F], s: Semigroup[W]): WriterT[W, F, B] =
    writerT(F.bind(run){wa =>
      val z = f(wa._2)
      F.map(z)(wb => (s.append(wa._1, wb._1), wb._2))
    })

  def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[WriterT[W, F, B]] = {
    G.map(F.traverse(run){
      case (w, a) => G.map(f(a))(b => (w, b))
    })(WriterT(_))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]): B =
    F.foldr(run, z) { a => b =>
      f(a._2, b)
    }

  def bimap[C, D](f: W => C, g: A => D)(implicit F: Functor[F]): WriterT[C, F, D] =
    writerT[F, C, D](F.map(run)({
      case (a, b) => (f(a), g(b))
    }))

  def leftMap[C](f: W => C)(implicit F: Functor[F]): WriterT[C, F, A] =
    bimap(f, identity)

  def bitraverse[G[_], C, D](f: W => G[C], g: A => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[WriterT[C, F, D]] =
    G.map(F.traverse[G, (W, A), (C, D)](run) {
      case (a, b) => G.tuple2(f(a), g(b))
    })(writerT(_))

  def rwst[R, S](implicit F: Functor[F]): ReaderWriterStateT[R, W, S, F, A] = ReaderWriterStateT(
    (r, s) => F.map(self.run) {
      case (w, a) => (w, a, s)
    }
  )

  def wpoint[G[_]](implicit F: Functor[F], P: Applicative[G]): WriterT[G[W], F, A] =
    writerT(F.map(self.run) {
      case (w, a) => (P.point(w), a)
    })

  def colocal[X](f: W => X)(implicit F: Functor[F]): WriterT[X, F, A] = mapWritten(f)
}

object WriterT extends WriterTInstances with WriterTFunctions

sealed abstract class WriterTInstances15 {
  implicit def writerTMonoid[W, F[_], A](implicit M: Monoid[F[(W,A)]]): Monoid[WriterT[W, F, A]] =
    new Monoid[WriterT[W, F, A]] {
      def zero = WriterT(M.zero)
      def append(a: WriterT[W, F, A], b: => WriterT[W, F, A]) = WriterT(M.append(a.run, b.run))
    }

  implicit def writerTPlus[W, F[_]](implicit F0: Plus[F]): Plus[WriterT[W, F, ?]] =
    new WriterTPlus[F, W] {
      def F = F0
    }
}

sealed abstract class WriterTInstances14 extends WriterTInstances15 {
  implicit def writerFunctor[W]: Functor[Writer[W, ?]] =
    new WriterTFunctor[Id, W] {
      implicit def F = idInstance
    }

  implicit def writerTPlusEmpty[W, F[_]](implicit F0: PlusEmpty[F]): PlusEmpty[WriterT[W, F, ?]] =
    new WriterTPlusEmpty[F, W] {
      def F = F0
    }
}
sealed abstract class WriterTInstances13 extends WriterTInstances14 {
  implicit def writerTFunctor[W, F[_]](implicit F0: Functor[F]): Functor[WriterT[W, F, ?]] =
    new WriterTFunctor[F, W] {
      implicit def F = F0
    }
}

sealed abstract class WriterTInstances12 extends WriterTInstances13 {
  implicit def writerBind[W](implicit W0: Semigroup[W]): Bind[Writer[W, ?]] =
    new WriterTBind[Id, W] {
      implicit def F = idInstance
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances11 extends WriterTInstances12 {
  implicit def writerTBindRec[W, F[_]](implicit W0: Semigroup[W], F0: BindRec[F], F1: Applicative[F]): BindRec[WriterT[W, F, ?]] =
    new WriterTBindRec[F, W] {
      implicit def F = F0
      implicit def A = F1
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances10 extends WriterTInstances11 {
  implicit def writerTApply[W, F[_]](implicit W0: Semigroup[W], F0: Apply[F]): Apply[WriterT[W, F, ?]] =
    new WriterTApply[F, W] {
      implicit def F = F0
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances9 extends WriterTInstances10 {
  implicit def writerTBind[W, F[_]](implicit W0: Semigroup[W], F0: Bind[F]): Bind[WriterT[W, F, ?]] =
    new WriterTBind[F, W] {
      implicit def F = F0
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances8 extends WriterTInstances9 {
  implicit def writerTApplicative[W, F[_]](implicit W0: Monoid[W], F0: Applicative[F]): Applicative[WriterT[W, F, ?]] =
    new WriterTApplicative[F, W] {
      implicit def F = F0
      implicit def W = W0
    }
}

sealed abstract class WriterTInstances7 extends WriterTInstances8 {
  implicit def writerBindRec[W](implicit W0: Semigroup[W]): BindRec[Writer[W, ?]] =
    new WriterTBindRec[Id, W] {
      implicit def F = idInstance
      implicit def A = idInstance
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
  implicit def writerTMonad[W, F[_]](implicit W0: Monoid[W], F0: Monad[F]): Monad[WriterT[W, F, ?]] =
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
  implicit def writerTEqual[F[_], W, A](implicit E: Equal[F[(W, A)]]): Equal[WriterT[W, F, A]] = E.contramap((_: WriterT[W, F, A]).run)

  implicit def writerTMonadError[F[_], E, W](implicit F0: MonadError[F, E], W0: Monoid[W]): MonadError[WriterT[W, F, ?], E] =
    new WriterTMonadError[F, E, W] {
      override def F = F0
      override def W = W0
    }
}

sealed abstract class WriterTInstances3 extends WriterTInstances4 {
  implicit def writerTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[WriterT[?, F, ?]] =
    new WriterTBifunctor[F] {
      implicit def F = F0
    }
  implicit def writerTFoldable[W, F[_]](implicit F0: Foldable[F]): Foldable[WriterT[W, F, ?]] =
    new WriterTFoldable[F, W] {
      implicit def F = F0
    }
  implicit def writerEqual[W, A](implicit E: Equal[(W, A)]): Equal[Writer[W, A]] = E.contramap((_: Writer[W, A]).run)

  implicit def writerTMonadPlus[W, F[_]](implicit W0: Monoid[W], F0: MonadPlus[F]): MonadPlus[WriterT[W, F, ?]] =
    new WriterTMonadPlus[F, W] {
      def F = F0
      def W = W0
    }
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
  implicit def writerTTraverse[W, F[_]](implicit F0: Traverse[F]): Traverse[WriterT[W, F, ?]] =
    new WriterTTraverse[F, W] {
      implicit def F = F0
    }
}

sealed abstract class WriterTInstances0 extends WriterTInstances1 {
  implicit def writerTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[WriterT[?, F, ?]] =
    new WriterTBitraverse[F] {
      implicit def F = F0
    }
  implicit def writerTraverse[W]: Traverse[Writer[W, ?]] =
    new WriterTTraverse[Id, W] {
      implicit def F = idInstance
    }
}

sealed abstract class WriterTInstances extends WriterTInstances0 {
  implicit def writerTMonadListen[W, F[_]](implicit F0: Monad[F], W0: Monoid[W]): MonadListen[WriterT[W, F, ?], W] =
    new WriterTMonadListen[F, W] {
      implicit def F = F0
      implicit def W = W0
    }

  implicit def writerTHoist[W](implicit W0: Monoid[W]): Hoist[λ[(α[_], β) => WriterT[W, α, β]]] =
    new WriterTHoist[W] {
      implicit def W = W0
    }

  implicit def writerTShow[F[_], W, A](implicit F0: Show[F[(W, A)]]): Show[WriterT[W, F, A]] =
    Contravariant[Show].contramap(F0)(_.run)
}

trait WriterTFunctions {
  def writerT[F[_], W, A](v: F[(W, A)]): WriterT[W, F, A] = WriterT(v)

  def writerTU[FAB, AB, A0, B0](fab: FAB)(implicit
                                          u1: Unapply[Functor, FAB]{type A = AB},
                                          @deprecated("scala/bug#5075", "") u2: Unapply2[Bifunctor, AB]{type A = A0; type B = B0},
                                          l: AB === (A0, B0)
  ): WriterT[A0, u1.M, B0] = WriterT(l.subst[u1.M](u1(fab)))

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT[Id, W, A](v)

  def tell[W](w: W): Writer[W, Unit] = writer((w, ()))

  def put[F[_], W, A](value: F[A])(w: W)(implicit F: Functor[F]): WriterT[W, F, A] =
    WriterT(F.map(value)(a => (w, a)))

  /** Puts the written value that is produced by applying the given function into a writer transformer and associates with `value` */
  def putWith[F[_], W, A](value: F[A])(w: A => W)(implicit F: Functor[F]): WriterT[W, F, A] =
    WriterT(F.map(value)(a => (w(a), a)))

}

//
// Type class implementation traits
//
import WriterT.writerT

private trait WriterTPlus[F[_], W] extends Plus[WriterT[W, F, ?]] {
  def F: Plus[F]
  override final def plus[A](a: WriterT[W, F, A], b: => WriterT[W, F, A]) =
    WriterT(F.plus(a.run, b.run))
}

private trait WriterTPlusEmpty[F[_], W] extends PlusEmpty[WriterT[W, F, ?]] with WriterTPlus[F, W] {
  def F: PlusEmpty[F]

  override final def empty[A] = WriterT(F.empty)
}

private trait WriterTFunctor[F[_], W] extends Functor[WriterT[W, F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WriterT[W, F, A])(f: A => B) = fa map f
}

private trait WriterTApply[F[_], W] extends Apply[WriterT[W, F, ?]] with WriterTFunctor[F, W] {
  implicit def F: Apply[F]
  implicit def W: Semigroup[W]

  override def ap[A, B](fa: => WriterT[W, F, A])(f: => WriterT[W, F, A => B]) = fa ap f
}

private trait WriterTApplicative[F[_], W] extends Applicative[WriterT[W, F, ?]] with WriterTApply[F, W] {
  implicit def F: Applicative[F]
  implicit def W: Monoid[W]
  def point[A](a: => A) = writerT(F.point((W.zero, a)))
}

private trait WriterTBind[F[_], W] extends Bind[WriterT[W, F, ?]] with WriterTApply[F, W] {
  implicit def F: Bind[F]

  override final def bind[A, B](fa: WriterT[W, F, A])(f: A => WriterT[W, F, B]) = fa flatMap f
}

private trait WriterTBindRec[F[_], W] extends BindRec[WriterT[W, F, ?]] with WriterTBind[F, W] {
  implicit def F: BindRec[F]
  implicit def A: Applicative[F]

  def tailrecM[A, B](a: A)(f: A => WriterT[W, F, A \/ B]): WriterT[W, F, B] = {
    def go(t: (W, A)): F[(W, A) \/ (W, B)] =
      F.map(f(t._2).run) {
        case (w0, e) =>
          val w1 = W.append(t._1, w0)
          e.bimap((w1, _), (w1, _))
      }

    WriterT(F.bind(f(a).run) {
      case (w, -\/(a0)) => F.tailrecM((w, a0))(go)
      case (w, \/-(b)) => A.point((w, b))
    })
  }
}

private trait WriterTMonad[F[_], W] extends Monad[WriterT[W, F, ?]] with WriterTApplicative[F, W] with WriterTBind[F, W] {
  implicit def F: Monad[F]
}

private trait WriterTMonadPlus[F[_], W] extends MonadPlus[WriterT[W, F, ?]] with WriterTMonad[F, W] with WriterTPlusEmpty[F, W] {
  def F: MonadPlus[F]
}

private trait WriterTMonadError[F[_], E, W] extends MonadError[WriterT[W, F, ?], E] with WriterTMonad[F, W] {
  implicit def F: MonadError[F, E]

  override def handleError[A](fa: WriterT[W, F, A])(f: E => WriterT[W, F, A]) =
    WriterT[W, F, A](F.handleError(fa.run)(f(_).run))

  override def raiseError[A](e: E) =
    WriterT[W, F, A](F.raiseError[(W, A)](e))
}

private trait WriterTFoldable[F[_], W] extends Foldable.FromFoldr[WriterT[W, F, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: WriterT[W, F, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
}

private trait WriterTTraverse[F[_], W] extends Traverse[WriterT[W, F, ?]] with WriterTFoldable[F, W] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: WriterT[W, F, A])(f: A => G[B]) = fa traverse f
}

private trait WriterTBifunctor[F[_]] extends Bifunctor[WriterT[?, F, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: WriterT[A, F, B])(f: A => C, g: B => D) =
    fab.bimap(f, g)
}

private trait WriterTBitraverse[F[_]] extends Bitraverse[WriterT[?, F, ?]] with WriterTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: WriterT[A, F, B])(f: A => G[C], g: B => G[D]) =
    fab.bitraverse(f, g)
}

private trait WriterComonad[W] extends Comonad[Writer[W, ?]] with WriterTFunctor[Id, W] {
  def copoint[A](p: Writer[W, A]): A = p.value

  override def cojoin[A](fa: Writer[W, A]): Writer[W, Writer[W, A]] =
    Writer(fa.written, fa)

  override def cobind[A, B](fa: Writer[W, A])(f: (Writer[W, A]) => B): Writer[W, B] =
    Writer(fa.written, f(fa))
}

private trait WriterTHoist[W] extends Hoist[λ[(α[_], β) => WriterT[W, α, β]]] {
  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): WriterT[W, M, B] =
    WriterT(M.map(mb)((W.zero, _)))

  implicit def W: Monoid[W]

  implicit def apply[M[_]: Monad]: Monad[WriterT[W, M, ?]] = WriterT.writerTMonad

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    λ[WriterT[W, M, ?] ~> WriterT[W, N, ?]](_ mapT f.apply)
}

private trait WriterTMonadListen[F[_], W] extends MonadListen[WriterT[W, F, ?], W] with WriterTMonad[F, W] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def writer[A](w: W, v: A): WriterT[W, F, A] = WriterT.writerT(F.point((w, v)))

  def listen[A](fa: WriterT[W, F, A]): WriterT[W, F, (A, W)] =
    WriterT(F.map(fa.run){ case (w, a) => (w, (a, w)) })
}
