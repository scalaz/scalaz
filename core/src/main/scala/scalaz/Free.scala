package scalaz

import annotation.tailrec
import Free._
// See explanation in comments on function1CovariantByName
import std.function.{function1Covariant => _, function1CovariantByName, _}
import std.tuple._

object Free extends FreeInstances {

  /** Collapse a trampoline to a single step. */
  def reset[A](r: Trampoline[A]): Trampoline[A] = { val a = r.run; return_(a) }

  /** Suspend the given computation in a single step. */
  def return_[S[_], A](value: => A)(implicit S: Applicative[S]): Free[S, A] =
    liftF[S, A](S.point(value))

  /** Alias for `point` */
  def pure[S[_], A](value: A): Free[S, A] = point(value)

  /** Absorb a step into the free monad. */
  def roll[S[_], A](value: S[Free[S, A]]): Free[S, A] =
    liftF(value).flatMap(x => x)

  /** Suspend a computation in a pure step of the applicative functor `S` */
  def suspend[S[_], A](value: => Free[S, A])(implicit S: Applicative[S]): Free[S, A] =
    liftF(S.pure(())).flatMap(_ => value)

  /** A version of `liftF` that infers the nested type constructor. */
  def liftFU[MA](value: => MA)(implicit MA: Unapply[Functor, MA]): Free[MA.M, MA.A] =
    liftF(MA(value))

  /** Monadic join for the higher-order monad `Free` */
  def joinF[S[_], A](value: Free[Free[S, ?], A]): Free[S, A] =
    value.flatMapSuspension(NaturalTransformation.refl[Free[S, ?]])

  /** A trampoline step that doesn't do anything. */
  def pause: Trampoline[Unit] =
    return_(())

  /** A source that produces the given value. */
  def produce[A](a: A): Source[A, Unit] =
    liftF[(A, ?), Unit]((a, ()))

  /** A sink that waits for a single value and returns it. */
  def await[A]: Sink[A, A] = liftF[(=> A) => ?, A](a => a)

  /** Absorb a step in `S` into the free monad for `S` */
  def apply[S[_], A](s: S[Free[S, A]]): Free[S, A] =
    roll(s)

  /** Return from the computation with the given value. */
  private case class Return[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  private case class Suspend[S[_], A](a: S[A]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  private case class Gosub[S[_], A0, B](a0: Free[S, A0], f0: A0 => Free[S, B]) extends Free[S, B] {
    type A = A0
    def a: Free[S, A] = a0
    def f: A => Free[S, B] = f0
  }

  /** A computation that can be stepped through, suspended, and paused */
  type Trampoline[A] = Free[Function0, A]

  /** A computation that produces values of type `A`, eventually resulting in a value of type `B`. */
  type Source[A, B] = Free[(A, ?), B]

  /** A computation that accepts values of type `A`, eventually resulting in a value of type `B`.
    * Note the similarity to an [[scalaz.iteratee.Iteratee]].
    */
  type Sink[A, B] = Free[(=> A) => ?, B]

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], A](value: S[A]): Free[S, A] =
    Suspend(value)

  /** Return the given value in the free monad. */
  def point[S[_], A](value: A): Free[S, A] = Return[S, A](value)

}

/**
 * A free monad for a type constructor `S`.
 * Binding is done using the heap instead of the stack, allowing tail-call elimination.
 */
sealed abstract class Free[S[_], A] {
  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Return(f(a)))

  /** Alias for `flatMap` */
  final def >>=[B](f: A => Free[S, B]): Free[S, B] = this flatMap f

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = Gosub(this, f)

  /** Catamorphism. Run the first given function if Return, otherwise, the second given function. */
  final def fold[B](r: A => B, s: S[Free[S, A]] => B)(implicit S: Functor[S]): B =
    resume.fold(s, r)

  /** Evaluates a single layer of the free monad **/
  @tailrec final def resume(implicit S: Functor[S]): (S[Free[S,A]] \/ A) =
    this match {
      case Return(a) => \/-(a)
      case Suspend(t) => -\/(S.map(t)(Return(_)))
      case b @ Gosub(_, _) => b.a match {
        case Return(a) => b.f(a).resume
        case Suspend(t) => -\/(S.map(t)(b.f))
        case c @ Gosub(_, _) => c.a.flatMap(z => c.f(z).flatMap(b.f)).resume
      }
    }

  /** Changes the suspension functor by the given natural transformation. */
  final def mapSuspension[T[_]](f: S ~> T): Free[T, A] =
    flatMapSuspension(λ[S ~> Free[T,?]](s => Suspend(f(s))))

  /** Modifies the first suspension with the given natural transformation. */
  final def mapFirstSuspension(f: S ~> S): Free[S, A] =
    step match {
      case Suspend(s) => Suspend(f(s))
      case a@Gosub(_, _) => a.a match {
        case Suspend(s) => Suspend(f(s)).flatMap(a.f)
        case _ => a.a.mapFirstSuspension(f).flatMap(a.f)
      }
      case x => x
    }

  /**
   * Substitutes a free monad over the given functor into the suspension functor of this program.
   * `Free` is a monad in an endofunctor category and this is its monadic bind.
   */
  final def flatMapSuspension[T[_]](f: S ~> Free[T, ?]): Free[T, A] =
    foldMap[Free[T,?]](f)(freeMonad[T])

  /** Applies a function `f` to a value in this monad and a corresponding value in the dual comonad, annihilating both. */
  final def zapWith[G[_], B, C](bs: Cofree[G, B])(f: (A, B) => C)(implicit S: Functor[S], d: Zap[S, G]): C =
    Zap.monadComonadZap.zapWith(this, bs)(f)

  /** Applies a function in a comonad to the corresponding value in this monad, annihilating both. */
  final def zap[G[_], B](fs: Cofree[G, A => B])(implicit S: Functor[S], d: Zap[S, G]): B =
    zapWith(fs)((a, f) => f(a))

  /** Runs a single step, using a function that extracts the resumption from its suspension functor. */
  final def bounce(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): Free[S, A] = resume match {
    case -\/(s) => f(s)
    case \/-(r) => Return(r)
  }

  /** Runs to completion, using a function that extracts the resumption from its suspension functor. */
  final def go(f: S[Free[S, A]] => Free[S, A])(implicit S: Functor[S]): A = {
    @tailrec def go2(t: Free[S, A]): A = t.resume match {
      case -\/(s) => go2(f(s))
      case \/-(r) => r
    }
    go2(this)
  }

  /**
   * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
   * @since 7.0.1
   */
  final def runM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Monad[M]): M[A] = {
    def runM2(t: Free[S, A]): M[A] = t.resume match {
      case -\/(s) => Monad[M].bind(f(s))(runM2)
      case \/-(r) => Monad[M].pure(r)
    }
    runM2(this)
  }

  /**
    * Run Free using constant stack.
    */
  final def runRecM[M[_]](f: S[Free[S, A]] => M[Free[S, A]])(implicit S: Functor[S], M: Applicative[M], B: BindRec[M]): M[A] = {
    def go(e: S[Free[S, A]] \/ A): M[Free[S, A] \/ A] =
      e match {
        case -\/(sf) => M.map(f(sf))(\/.left)
        case a @ \/-(_) => M.point(a)
      }

    B.tailrecM[Free[S, A], A]((ma: Free[S, A]) => go(ma.resume))(this)
  }

  /**
   * Evaluate one layer in the free monad, re-associating any left-nested binds to the right
   * and pulling the first suspension to the top.
   */
  @annotation.tailrec final def step: Free[S, A] = this match {
    case x@Gosub(_, _) => x.a match {
      case b@Gosub(_, _) =>
        b.a.flatMap(a => b.f(a).flatMap(x.f)).step
      case Return(b)=>
        x.f(b).step
      case _ =>
        x
    }
    case x => x
  }

  /**
   * Catamorphism for `Free`.
   * Runs to completion, mapping the suspension with the given transformation at each step and
   * accumulating into the monad `M`.
   */
  final def foldMap[M[_]](f: S ~> M)(implicit M: Monad[M]): M[A] =
    step match {
      case Return(a) => M.pure(a)
      case Suspend(s) => f(s)
      // This is stack safe because `step` ensures right-associativity of Gosub
      case a@Gosub(_, _) => M.bind(a.a foldMap f)(c => a.f(c) foldMap f)
    }

  final def foldMapRec[M[_]](f: S ~> M)(implicit M: Applicative[M], B: BindRec[M]): M[A] =
    B.tailrecM[Free[S, A], A]{
      _.step match {
        case Return(a) => M.point(\/-(a))
        case Suspend(t) => M.map(f(t))(\/.right)
        case b @ Gosub(_, _) => (b.a: @unchecked) match {
          case Suspend(t) => M.map(f(t))(a => -\/(b.f(a)))
        }
      }
    }(this)

  import Id._

  /**
   * Folds this free recursion to the right using the given natural transformations.
   */
  final def foldRight[G[_]](z: Id ~> G)(f: λ[α => S[G[α]]] ~> G)(implicit S: Functor[S]): G[A] =
    this.resume match {
      case -\/(s) => f(S.map(s)(_.foldRight(z)(f)))
      case \/-(r) => z(r)
    }

  /** Runs to completion, allowing the resumption function to thread an arbitrary state of type `B`. */
  final def foldRun[B](b: B)(f: (B, S[Free[S, A]]) => (B, Free[S, A]))(implicit S: Functor[S]): (B, A) = {
    @tailrec def foldRun2(t: Free[S, A], z: B): (B, A) = t.resume match {
      case -\/(s) =>
        val (b1, s1) = f(z, s)
        foldRun2(s1, b1)
      case \/-(r) => (z, r)
    }
    foldRun2(this, b)
  }

  /** Variant of `foldRun` that allows to interleave effect `M` at each step. */
  final def foldRunM[M[_], B](b: B)(f: λ[α => (B, S[α])] ~> λ[α => M[(B, α)]])(implicit M0: Applicative[M], M1: BindRec[M]): M[(B, A)] =
    M1.tailrecM[(B, Free[S, A]), (B, A)]{ case (b, fa) =>
      fa.step match {
        case Return(a) => M0.point(\/-((b, a)))
        case Suspend(sa) => M0.map(f((b, sa)))(\/.right)
        case g @ Gosub(_, _) => g.a match {
          case Suspend(sz) =>
            M0.map(f((b, sz))) { case (b, z) => -\/((b, g.f(z))) }
          case _ => sys.error("Unreachable code: `Gosub` returned from `step` must have `Suspend` on the left")
        }
      }
    }((b, this))

  /** Runs a trampoline all the way to the end, tail-recursively. */
  final def run(implicit ev: Free[S, A] =:= Trampoline[A]): A =
    ev(this).go(_())

  /** Interleave this computation with another, combining the results with the given function. */
  final def zipWith[B, C](tb: Free[S, B])(f: (A, B) => C): Free[S, C] = {
    (step, tb.step) match {
      case (Return(a), Return(b)) => Return(f(a, b))
      case (a@Suspend(_), Return(b)) => a.flatMap(x => Return(f(x, b)))
      case (Return(a), b@Suspend(_)) => b.flatMap(x => Return(f(a, x)))
      case (a@Suspend(_), b@Suspend(_)) => a.flatMap(x => b.map(y => f(x, y)))
      case (a@Gosub(_, _), Return(b)) => a.a.flatMap(x => a.f(x).map(f(_, b)))
      case (a@Gosub(_, _), b@Suspend(_)) => a.a.flatMap(x => b.flatMap(y => a.f(x).map(f(_, y))))
      case (a@Gosub(_, _), b@Gosub(_, _)) => a.a.zipWith(b.a)((x, y) => a.f(x).zipWith(b.f(y))(f)).flatMap(x => x)
      case (a, b@Gosub(_, _)) => a.flatMap(x => b.a.flatMap(y => b.f(y).map(f(x, _))))
    }
  }

  /** Runs a `Source` all the way to the end, tail-recursively, collecting the produced values. */
  def collect[B](implicit ev: Free[S, A] =:= Source[B, A]): (Vector[B], A) = {
    @tailrec def go(c: Source[B, A], v: Vector[B] = Vector()): (Vector[B], A) =
      c.resume match {
        case -\/((b, cont)) => go(cont, v :+ b)
        case \/-(r)         => (v, r)
      }
    go(ev(this))
  }

  /** Drive this `Source` with the given Sink. */
  def drive[E, B](sink: Sink[Option[E], B])(implicit ev: Free[S, A] =:= Source[E, A]): (A, B) = {
    @tailrec def go(src: Source[E, A], snk: Sink[Option[E], B]): (A, B) =
      (src.resume, snk.resume) match {
        case (-\/((e, c)), -\/(f)) => go(c, f(Some(e)))
        case (-\/((e, c)), \/-(y)) => go(c, Sink.sinkMonad[Option[E]].pure(y))
        case (\/-(x), -\/(f))      => go(Source.sourceMonad[E].pure(x), f(None))
        case (\/-(x), \/-(y))      => (x, y)
      }
    go(ev(this), sink)
  }

  /** Feed the given stream to this `Source`. */
  def feed[E](ss: Stream[E])(implicit ev: Free[S, A] =:= Sink[E, A]): A = {
    @tailrec def go(snk: Sink[E, A], rest: Stream[E]): A = (rest, snk.resume) match {
      case (x #:: xs, -\/(f)) => go(f(x), xs)
      case (Stream(), -\/(f)) => go(f(sys.error("No more values.")), Stream())
      case (_, \/-(r))        => r
    }
    go(ev(this), ss)
  }

  /** Feed the given source to this `Sink`. */
  def drain[E, B](source: Source[E, B])(implicit ev: Free[S, A] =:= Sink[E, A]): (A, B) = {
    @tailrec def go(src: Source[E, B], snk: Sink[E, A]): (A, B) = (src.resume, snk.resume) match {
      case (-\/((e, c)), -\/(f)) => go(c, f(e))
      case (-\/((e, c)), \/-(y)) => go(c, Sink.sinkMonad[E].pure(y))
      case (\/-(x), -\/(f))      => sys.error("Not enough values in source.")
      case (\/-(x), \/-(y))      => (y, x)
    }
    go(source, ev(this))
  }

  /** Duplication in `Free` as a comonad in the endofunctor category. */
  def duplicateF: Free[Free[S, ?], A] = extendF[Free[S,?]](NaturalTransformation.refl[Free[S,?]])

  /** Extension in `Free` as a comonad in the endofunctor category. */
  def extendF[T[_]](f: Free[S, ?] ~> T): Free[T, A] = mapSuspension(λ[S ~> T](x => f(liftF(x))))

  /** Extraction from `Free` as a comonad in the endofunctor category. */
  def extractF(implicit S: Monad[S]): S[A] = foldMap(NaturalTransformation.refl[S])

  def toFreeT(implicit S: Functor[S]): FreeT[S, Id, A] =
    this match {
      case Return(a) =>
        FreeT.point(a)
      case Suspend(a) =>
        FreeT.liftF(a)
      case a @ Gosub(_, _) =>
        a.a.toFreeT.flatMap(a.f.andThen(_.toFreeT))
    }
}

object Trampoline extends TrampolineInstances {

  def done[A](a: A): Trampoline[A] =
    Free.pure[Function0,A](a)

  def delay[A](a: => A): Trampoline[A] =
    suspend(done(a))

  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    Free.suspend(a)
}

sealed trait TrampolineInstances {
  implicit val trampolineInstance: Monad[Trampoline] with Comonad[Trampoline] with BindRec[Trampoline] =
    new Monad[Trampoline] with Comonad[Trampoline] with BindRec[Trampoline] {
      override def point[A](a: => A) = return_[Function0, A](a)
      def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
      def copoint[A](fa: Trampoline[A]) = fa.run
      def cobind[A, B](fa: Trampoline[A])(f: Trampoline[A] => B) = return_(f(fa))
      override def cojoin[A](fa: Trampoline[A]) = Free.point(fa)
      def tailrecM[A, B](f: A => Trampoline[A \/ B])(a: A): Trampoline[B] =
        f(a).flatMap(_.fold(tailrecM(f), point(_)))
    }
}

object Sink extends SinkInstances

sealed trait SinkInstances {
  implicit def sinkMonad[S]: Monad[Sink[S, ?]] =
    new Monad[Sink[S, ?]] {
      def point[A](a: => A) = liftF[(=> S) => ?, Unit](s => ()).map(_ => a)
      def bind[A, B](s: Sink[S, A])(f: A => Sink[S, B]) = s flatMap f
    }
}

object Source extends SourceInstances

sealed trait SourceInstances {
  implicit def sourceMonad[S]: Monad[Source[S, ?]] =
    new Monad[Source[S, ?]] {
      override def point[A](a: => A) = Free.point[(S, ?), A](a)
      def bind[A, B](s: Source[S, A])(f: A => Source[S, B]) = s flatMap f
    }
}

sealed abstract class FreeInstances3 {
  implicit def freeFoldable[F[_]: Foldable: Functor]: Foldable[Free[F, ?]] =
    new FreeFoldable[F] {
      def F = implicitly
      def F0 = implicitly
    }
}

sealed abstract class FreeInstances2 extends FreeInstances3 {
  implicit def freeFoldable1[F[_]: Foldable1: Functor]: Foldable1[Free[F, ?]] =
    new FreeFoldable1[F] {
      def F = implicitly
      def F0 = implicitly
    }
}

sealed abstract class FreeInstances1 extends FreeInstances2 {
  implicit def freeTraverse[F[_]: Traverse]: Traverse[Free[F, ?]] =
    new FreeTraverse[F] {
      def F = implicitly
    }
}

sealed abstract class FreeInstances0 extends FreeInstances1 {
  implicit def freeTraverse1[F[_]: Traverse1]: Traverse1[Free[F, ?]] =
    new FreeTraverse1[F] {
      def F = implicitly
    }

  implicit def freeSemigroup[S[_], A: Semigroup]: Semigroup[Free[S, A]] =
    Semigroup.liftSemigroup[Free[S, ?], A]
}

// Trampoline, Sink, and Source are type aliases. We need to add their type class instances
// to Free to be part of the implicit scope.
sealed abstract class FreeInstances extends FreeInstances0 with TrampolineInstances with SinkInstances with SourceInstances {
  implicit def freeMonad[S[_]]: Monad[Free[S, ?]] with BindRec[Free[S, ?]] =
    new Monad[Free[S, ?]] with BindRec[Free[S, ?]] {
      override def map[A, B](fa: Free[S, A])(f: A => B) = fa map f
      def bind[A, B](a: Free[S, A])(f: A => Free[S, B]) = a flatMap f
      def point[A](a: => A) = Free.point(a)
      // Free trampolines, should be alright to just perform binds.
      def tailrecM[A, B](f: A => Free[S, A \/ B])(a: A): Free[S, B] =
        f(a).flatMap(_.fold(tailrecM(f), point(_)))
    }

  implicit def freeZip[S[_]](implicit F: Functor[S], Z: Zip[S]): Zip[Free[S, ?]] =
    new Zip[Free[S, ?]] {
      override def zip[A, B](aa: => Free[S, A], bb: => Free[S, B]) =
        (aa.resume, bb.resume) match {
          case (-\/(a), -\/(b)) => roll(Z.zipWith(a, b)(zip(_, _)))
          case (-\/(a), \/-(b)) => roll(F.map(a)(zip(_, point(b))))
          case (\/-(a), -\/(b)) => roll(F.map(b)(zip(point(a), _)))
          case (\/-(a), \/-(b)) => point((a, b))
        }
    }

  implicit def freeMonoid[S[_], A: Monoid]: Monoid[Free[S, A]] =
    Monoid.liftMonoid[Free[S, ?], A]
}

private sealed trait FreeBind[F[_]] extends Bind[Free[F, ?]] {
  override def map[A, B](fa: Free[F, A])(f: A => B) = fa map f
  def bind[A, B](a: Free[F, A])(f: A => Free[F, B]) = a flatMap f
}

private sealed trait FreeFoldable[F[_]] extends Foldable[Free[F, ?]] {
  def F: Foldable[F]
  implicit def F0: Functor[F]

  override final def foldMap[A, B: Monoid](fa: Free[F, A])(f: A => B): B =
    fa.resume match {
      case -\/(s) => F.foldMap(s)(foldMap(_)(f))
      case \/-(r) => f(r)
    }

  override final def foldLeft[A, B](fa: Free[F, A], z: B)(f: (B, A) => B): B =
    fa.resume match {
      case -\/(s) => F.foldLeft(s, z)((b, a) => foldLeft(a, b)(f))
      case \/-(r) => f(z, r)
    }

  override final def foldRight[A, B](fa: Free[F, A], z: => B)(f: (A, => B) => B): B =
    fa.resume match {
      case -\/(s) => F.foldRight(s, z)(foldRight(_, _)(f))
      case \/-(r) => f(r, z)
    }
}

private sealed trait FreeFoldable1[F[_]] extends Foldable1[Free[F, ?]] {
  def F: Foldable1[F]
  implicit def F0: Functor[F]

  override final def foldMap1[A, B: Semigroup](fa: Free[F, A])(f: A => B): B =
    fa.resume match {
      case -\/(s) => F.foldMap1(s)(foldMap1(_)(f))
      case \/-(r) => f(r)
    }

  override final def foldMapRight1[A, B](fa: Free[F, A])(z: A => B)(f: (A, => B) => B): B =
    fa.resume match {
      case -\/(s) => F.foldMapRight1(s)(foldMapRight1(_)(z)(f))(foldRight(_, _)(f))
      case \/-(r) => z(r)
    }

  override final def foldMapLeft1[A, B](fa: Free[F, A])(z: A => B)(f: (B, A) => B): B =
    fa.resume match {
      case -\/(s) => F.foldMapLeft1(s)(foldMapLeft1(_)(z)(f))((b, a) => foldLeft(a, b)(f))
      case \/-(r) => z(r)
    }
}

private sealed trait FreeTraverse[F[_]] extends Traverse[Free[F, ?]] with FreeFoldable[F]{
  implicit def F: Traverse[F]
  override final def F0 = F

  override final def map[A, B](fa: Free[F, A])(f: A => B) = fa map f

  override final def traverseImpl[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Free[F, B]] =
    fa.resume match {
      case -\/(s) => G.map(F.traverseImpl(s)(traverseImpl[G, A, B](_)(f)))(roll(_))
      case \/-(r) => G.map(f(r))(point(_))
    }
}

private sealed abstract class FreeTraverse1[F[_]] extends Traverse1[Free[F, ?]] with FreeTraverse[F] with FreeFoldable1[F]{
  implicit def F: Traverse1[F]

  override final def traverse1Impl[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Apply[G]): G[Free[F, B]] =
    fa.resume match {
      case -\/(s) => G.map(F.traverse1Impl(s)(traverse1Impl[G, A, B](_)(f)))(roll(_))
      case \/-(r) => G.map(f(r))(point(_))
    }
}
