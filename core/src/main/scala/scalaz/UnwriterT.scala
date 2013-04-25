package scalaz

import Id._

/**
 * This data type is isomorphic to `WriterT`, however, it is NOT a monad.
 *
 * It implements flatMap+map and drops the write value. There is no `Monoid` or `Semigroup` required. There is no `point` operation.
 * You can switch between `WriterT` and `UnwriterT` with `unary_+` and `unary_-`.
 */
sealed trait UnwriterT[F[+_], +U, +A] { self =>
  val run: F[(U, A)]

  import UnwriterT._

  def on: WriterT[F, U, A] =
    WriterT(run)

  /** alias for `on` */
  def unary_+ : WriterT[F, U, A] =
    WriterT(run)

  def mapValue[X, B](f: ((U, A)) => (X, B))(implicit F: Functor[F]): UnwriterT[F, X, B] =
    unwriterT(F.map(run)(f))

  def mapUnwritten[X](f: U => X)(implicit F: Functor[F]): UnwriterT[F, X, A] =
    mapValue(wa => (f(wa._1), wa._2))

  def unwritten(implicit F: Functor[F]): F[U] =
    F.map(run)(_._1)

  def value(implicit F: Functor[F]): F[A] =
    F.map(run)(_._2)

  def swap(implicit F: Functor[F]): UnwriterT[F, A, U] =
    mapValue(wa => (wa._2, wa._1))

  def map[B](f: A => B)(implicit F: Functor[F]): UnwriterT[F, U, B] =
    unwriterT(F.map(run)(wa => (wa._1, f(wa._2))))

  def foreach[B](f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(wa => f(wa._2))

  def ap[B, UU >: U](f: => UnwriterT[F, UU, A => B])(implicit F: Apply[F]): UnwriterT[F, UU, B] =
    unwriterT {
      F.apply2(f.run, run) {
        case ((w1, fab), (_, a)) => (w1, fab(a))
      }
    }

  def flatMap[B, UU >: U](f: A => UnwriterT[F, UU, B])(implicit F: Bind[F]): UnwriterT[F, UU, B] =
    unwriterT(F.bind(run){wa =>
      val z = f(wa._2).run
      F.map(z)(wb => (wa._1, wb._2))
    })

  def traverse[G[_] , UU >: U, B](f: A => G[B])(implicit G: Applicative[G], F: Traverse[F]): G[UnwriterT[F, UU, B]] = {
    G.map(F.traverse(run){
      case (w, a) => G.map(f(a))(b => (w, b))
    })(UnwriterT(_))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]) =
    F.foldr(run, z) { a => b =>
      f(a._2, b)
    }

  def bimap[C, D](f: U => C, g: A => D)(implicit F: Functor[F]) =
    unwriterT[F, C, D](F.map(run)({
      case (a, b) => (f(a), g(b))
    }))

  def leftMap[C](f: U => C)(implicit F: Functor[F]): UnwriterT[F, C, A] =
    bimap(f, identity)

  def bitraverse[G[_], C, D](f: U => G[C], g: A => G[D])(implicit G: Applicative[G], F: Traverse[F]) =
    G.map(F.traverse[G, (U, A), (C, D)](run) {
      case (a, b) => G.apply2(f(a), g(b))((_, _))
    })(unwriterT(_))

  def wpoint[G[+_]](implicit F: Functor[F], P: Applicative[G]): UnwriterT[F, G[U], A] =
    unwriterT(F.map(self.run) {
      case (u, a) => (P.point(u), a)
    })

  def colocal[X](f: U => X)(implicit F: Functor[F]): UnwriterT[F, X, A] = mapUnwritten(f)
}

object UnwriterT extends UnwriterTFunctions with UnwriterTInstances {
  def apply[F[+_], W, A](v: F[(W, A)]): UnwriterT[F, W, A] =
    unwriterT(v)
}

trait UnwriterTInstances2 {
  implicit def unwriterTFunctor[F[+_], W](implicit F0: Functor[F]) = new UnwriterTFunctor[F, W] {
    implicit def F = F0
  }
}

trait UnwriterTInstances1 extends UnwriterTInstances2 {
  implicit def unwriterTApply[F[+_], W](implicit F0: Apply[F]) = new UnwriterTApply[F, W] {
    implicit def F = F0
  }
}

trait UnwriterTInstances0 extends UnwriterTInstances1 {
  implicit def unwriterTBifunctor[F[+_]](implicit F0: Functor[F]) = new UnwriterTBifunctor[F] {
    implicit def F = F0
  }
  implicit def unwriterTBind[F[+_], W](implicit F0: Bind[F]) = new UnwriterTBind[F, W] {
    implicit def F = F0
  }
  implicit def unwriterTFoldable[F[+_], W](implicit F0: Foldable[F]) = new UnwriterTFoldable[F, W] {
    implicit def F = F0
  }
  implicit def unwriterTEqual[F[+_], W, A](implicit E: Equal[F[(W, A)]]) = E.contramap((_: UnwriterT[F, W, A]).run)
}

trait UnwriterTInstances extends UnwriterTInstances0 {
  implicit def unwriterTBitraverse[F[+_]](implicit F0: Traverse[F]) = new UnwriterTBitraverse[F] {
    implicit def F = F0
  }
  implicit def unwriterComonad[W] = new UnwriterComonad[W] {
    implicit def F = implicitly
  }
  implicit def unwriterTTraverse[F[+_], W](implicit F0: Traverse[F]) = new UnwriterTTraverse[F, W] {
    implicit def F = F0
  }
  implicit def unwriterTIndex[W] = new UnwriterTIndex[W] {
  }
  implicit def unwriterTEach[F[+_], W](implicit F0: Each[F]) = new UnwriterTEach[F, W] {
    implicit def F = F0
  }
  implicit def unwriterEqual[W, A](implicit W: Equal[W], A: Equal[A]) = {
    import std.tuple._
    Equal[(W, A)].contramap((_: Unwriter[W, A]).run)
  }
}

trait UnwriterTFunctions {
  def unwriterT[F[+_], W, A](v: F[(W, A)]): UnwriterT[F, W, A] = new UnwriterT[F, W, A] {
    val run = v
  }

  import StoreT._

  def unwriter[W, A](v: (W, A)): Unwriter[W, A] =
    unwriterT[Id, W, A](v)

  def tell[W](w: W): Unwriter[W, Unit] = unwriter(w -> ())

  def unput[F[+_], W, A](value: F[A])(w: W)(implicit F: Functor[F]): UnwriterT[F, W, A] =
    UnwriterT(F.map(value)(a => (w, a)))

  /** Puts the written value that is produced by applying the given function into a unwriter transformer and associates with `value` */
  def unputWith[F[+_], W, A](value: F[A])(w: A => W)(implicit F: Functor[F]): UnwriterT[F, W, A] =
    UnwriterT(F.map(value)(a => (w(a), a)))

}

//
// Type class implementation traits
//

private[scalaz] trait UnwriterTFunctor[F[+_], W] extends Functor[({type λ[+α]=UnwriterT[F, W, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: UnwriterT[F, W, A])(f: A => B) = fa map f
}

private[scalaz] trait UnwriterTApply[F[+_], W] extends Apply[({type λ[+α]=UnwriterT[F, W, α]})#λ] with UnwriterTFunctor[F, W] {
  implicit def F: Apply[F]

  override def ap[A, B](fa: => UnwriterT[F, W, A])(f: => UnwriterT[F, W, A => B]) = fa ap f
}

private[scalaz] trait UnwriterTEach[F[+_], W] extends Each[({type λ[+α]=UnwriterT[F, W, α]})#λ] {
  implicit def F: Each[F]

  def each[A](fa: UnwriterT[F, W, A])(f: A => Unit) = fa foreach f
}

// TODO does Index it make sense for F other than Id?
private[scalaz] trait UnwriterTIndex[W] extends Index[({type λ[+α]=UnwriterT[Id, W, α]})#λ] {
  def index[A](fa: UnwriterT[Id, W, A], i: Int) = if(i == 0) Some(fa.value) else None
}

private[scalaz] trait UnwriterTBind[F[+_], W] extends Bind[({type λ[+α]=UnwriterT[F, W, α]})#λ] with UnwriterTApply[F, W] {
  implicit def F: Bind[F]

  def bind[A, B](fa: UnwriterT[F, W, A])(f: A => UnwriterT[F, W, B]) = fa flatMap f
}

private[scalaz] trait UnwriterTFoldable[F[+_], W] extends Foldable.FromFoldr[({type λ[+α]=UnwriterT[F, W, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: UnwriterT[F, W, A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
}

private[scalaz] trait UnwriterTTraverse[F[+_], W] extends Traverse[({type λ[+α]=UnwriterT[F, W, α]})#λ] with UnwriterTFoldable[F, W] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: UnwriterT[F, W, A])(f: A => G[B]) = fa traverse f
}

private[scalaz] trait UnwriterTBifunctor[F[+_]] extends Bifunctor[({type λ[+α, +β]=UnwriterT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: UnwriterT[F, A, B])(f: A => C, g: B => D) =
    fab.bimap(f, g)
}

private[scalaz] trait UnwriterTBitraverse[F[+_]] extends Bitraverse[({type λ[+α, +β]=UnwriterT[F, α, β]})#λ] with UnwriterTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: UnwriterT[F, A, B])(f: A => G[C], g: B => G[D]) =
    fab.bitraverse(f, g)
}

private[scalaz] trait UnwriterComonad[W] extends Comonad[({type λ[+α] = Unwriter[W, α]})#λ] with UnwriterTFunctor[Id, W] {

  override def cojoin[A](fa: Unwriter[W, A]): Unwriter[W, Unwriter[W, A]] =
    Unwriter(fa.unwritten, fa)

  override def cobind[A, B](fa: Unwriter[W, A])(f: (Unwriter[W, A]) => B): Unwriter[W, B] =
    Unwriter(fa.unwritten, f(fa))
  def copoint[A](p: Unwriter[W, A]): A = p.value

}
