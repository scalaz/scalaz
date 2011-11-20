package scalaz

import Isomorphism.<~>

sealed trait WriterT[F[_], W, A] {
  val runT: F[(W, A)]

  import WriterT._

  def run(implicit i: F <~> Id): (W, A) =
    i.to(runT)

  def mapValue[X, B](f: ((W, A)) => (X, B))(implicit F: Functor[F]): WriterT[F, X, B] =
    writerT(F.map(runT)(f))

  def mapWritten[X](f: W => X)(implicit ftr: Functor[F]): WriterT[F, X, A] =
    mapValue(wa => (f(wa._1), wa._2))

  def writtenT(implicit F: Functor[F]): F[W] =
    F.map(runT)(_._1)

  def written(implicit i: F <~> Id): W =
    run._1

  def overT(implicit F: Functor[F]): F[A] =
    F.map(runT)(_._2)

  def over(implicit i: F <~> Id): A =
    run._2

  def swapT(implicit F: Functor[F]): WriterT[F, A, W] =
    mapValue(wa => (wa._2, wa._1))

  def swap(implicit i: F <~> Id): Writer[A, W] = {
    val (w, a) = run
    writer((a, w))
  }

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
    writerT(F.map(runT)(wa => (wa._1, f(wa._2))))

  def foreach[B](f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(runT)(wa => f(wa._2))

  def ap[B](f: => WriterT[F, W, (A) => B])(implicit F: Apply[F], W: Semigroup[W]): WriterT[F, W, B] = writerT {
    F.map2(f.runT, runT) {
      case ((w1, fab), (w2, a)) => (W.append(w1, w2), fab(a))
    }
  }

  def flatMap[B](f: A => WriterT[F, W, B])(implicit F: Monad[F], s: Semigroup[W]): WriterT[F, W, B] =
    writerT(F.bind(runT){wa =>
      val z = f(wa._2).runT
      F.map(z)(wb => (s.append(wa._1, wb._1), wb._2))
    })

  def traverse[G[_] , B](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[WriterT[F, W, B]] = {
    G.map(F.traverse(runT){
      case (w, a) => G.map(f(a))(b => (w, b))
    })(WriterT(_))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]) =
    F.foldR(runT, z) { a => b =>
      f(a._2, b)
    }

  def bimap[C, D](f: (W) => C, g: (A) => D)(implicit F: Functor[F]) =
    writerT[F, C, D](F.map(runT)({
      case (a, b) => (f(a), g(b))
    }))

  def bitraverse[G[_], C, D](f: (W) => G[C], g: (A) => G[D])(implicit G: Applicative[G], F: Traverse[F]) =
    G.map(F.traverse[G, (W, A), (C, D)](runT) {
      case (a, b) => G.map2(f(a), g(b))((_, _))
    })(writerT(_))

//  def rws[R, S](implicit ftr: Functor[F]): ReaderWriterStateT[A, W, S, F, A] =
//    ReaderWriterStateT.readerWriterStateT(_ => s =>
//      implicitly[Functor[F]].fmap((wa: (W, A)) => (wa._2, s, wa._1))(runT))

}

object WriterT extends WriterTFunctions with WriterTInstances {
  def apply[F[_], W, A](v: F[(W, A)]): WriterT[F, W, A] =
    writerT(v)
}

trait WriterTInstances4 {
  implicit def writerTFunctor[F[_], W](F0: Functor[F]) = new WriterTFunctor[F, W] {
    implicit def F = F0
  }
}

trait WriterTInstances3 extends WriterTInstances4 {
  implicit def writerTPointed[F[_], W](implicit W0: Monoid[W], F0: Pointed[F]) = new WriterTPointed[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

trait WriterTInstances2 extends WriterTInstances3 {
  implicit def writerTApply[F[_], W](implicit W0: Semigroup[W], F0: Apply[F]) = new WriterTApply[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

trait WriterTInstances1 extends WriterTInstances2 {
  implicit def writerTApplicative[F[_], W](implicit W0: Monoid[W], F0: Applicative[F]) = new WriterTApplicative[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
}

trait WriterTInstances0 extends WriterTInstances1 {
  implicit def writerTBiFunctor[F[_]](implicit F0: Functor[F]) = new WriterTBiFunctor[F] {
    implicit def F = F0
  }
  implicit def writerTMonad[F[_], W](implicit W0: Monoid[W], F0: Monad[F]) = new WriterTMonad[F, W] {
    implicit def F = F0
    implicit def W = W0
  }
  implicit def writerTFoldable[F[_], W](implicit F0: Foldable[F]) = new WriterTFoldable[F, W] {
    implicit def F = F0
  }
}

trait WriterTInstances extends WriterTInstances0 {
  implicit def writerTBiTraverse[F[_]](implicit F0: Traverse[F]) = new WriterTBiTraverse[F] {
    implicit def F = F0
  }
  implicit def writerTTraverse[F[_], W](implicit F0: Traverse[F]) = new WriterTTraverse[F, W] {
    implicit def F = F0
  }
  implicit def writerTIndex[F[_], W](implicit F0: Index[F], i: F <~> Id) = new WriterTIndex[F, W] {
    implicit def iso = i
    implicit def F = F0
  }
  implicit def writerTEach[F[_], W](implicit F0: Each[F]) = new WriterTEach[F, W] {
    implicit def F = F0
  }
}

trait WriterTFunctions {
  type Writer[W, A] = WriterT[Id, W, A]
  def Writer[W, A](w: W, a: A): WriterT[Id, W, A] = WriterT[Id, W, A]((w, a))

  def writerT[F[_], W, A](v: F[(W, A)]): WriterT[F, W, A] = new WriterT[F, W, A] {
    val runT = v
  }

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT[Id, W, A](v)

  def tell[W](w: W): Writer[W, Unit] = writer(w -> ())
}

//
// Type class implementation traits
//
import WriterT.writerT

trait WriterTFunctor[F[_], W] extends Functor[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: WriterT[F, W, A])(f: (A) => B) = fa map f
}

trait WriterTPointed[F[_], W] extends Pointed[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTFunctor[F, W] {
  implicit def F: Pointed[F]
  implicit def W: Monoid[W]

  def point[A](a: => A) = writerT(F.point((W.zero, a)))
}

trait WriterTApply[F[_], W] extends Apply[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTFunctor[F, W] {
  implicit def F: Apply[F]
  implicit def W: Semigroup[W]

  override def ap[A, B](fa: WriterT[F, W, A])(f: => WriterT[F, W, (A) => B]) = fa ap f
}

trait WriterTApplicative[F[_], W] extends Applicative[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTApply[F, W] with WriterTPointed[F, W] {
  implicit def F: Applicative[F]
  implicit def W: Monoid[W]
}

trait WriterTEach[F[_], W] extends Each[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def F: Each[F]

  def each[A](fa: WriterT[F, W, A])(f: (A) => Unit) = fa foreach f
}

// TODO does Index it make sense for F other than Id?
trait WriterTIndex[F[_], W] extends Index[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def iso: F <~> Id
  def index[A](fa: WriterT[F, W, A], i: Int) = if(i == 0) Some(fa.over) else None
}

trait WriterTMonad[F[_], W] extends Monad[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTApplicative[F, W] with WriterTPointed[F, W] {
  implicit def F: Monad[F]

  def bind[A, B](fa: WriterT[F, W, A])(f: (A) => WriterT[F, W, B]) = fa flatMap f
}

trait WriterTFoldable[F[_], W] extends Foldable.FromFoldr[({type λ[α]=WriterT[F, W, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: WriterT[F, W, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
}

trait WriterTTraverse[F[_], W] extends Traverse[({type λ[α]=WriterT[F, W, α]})#λ] with WriterTFoldable[F, W] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: WriterT[F, W, A])(f: (A) => G[B]) = fa traverse f
}

trait WriterTBiFunctor[F[_]] extends BiFunctor[({type λ[α, β]=WriterT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: WriterT[F, A, B])(f: (A) => C, g: (B) => D) =
    fab.bimap(f, g)
}

trait WriterTBiTraverse[F[_]] extends BiTraverse[({type λ[α, β]=WriterT[F, α, β]})#λ] with WriterTBiFunctor[F] {
  implicit def F: Traverse[F]

  def bitraverse[G[_]: Applicative, A, B, C, D](fab: WriterT[F, A, B])(f: (A) => G[C], g: (B) => G[D]) =
    fab.bitraverse(f, g)
}
