package scalaz

import Id._

/**
 * StreamT monad transformer.
 */
sealed class StreamT[M[_], A](val step: M[StreamT.Step[A, StreamT[M, A]]]) {

  import StreamT._

  def uncons(implicit M: Monad[M]): M[Option[(A, StreamT[M, A])]] =
    M.bind(step) (
      _( yieldd = (a, s) => M.point(Some((a, s)))
       , skip = s => s.uncons
       , done = M.point(None)
       ))

  def unconsRec(implicit M: BindRec[M]): M[Option[(A, StreamT[M, A])]] = {
    def proceed(s: StreamT[M, A]): M[StreamT[M, A] \/ Option[(A, StreamT[M, A])]] =
      M.map(s.step) (
        _( yieldd = (a, s) => \/-(Some((a, s)))
         , skip = s => -\/(s)
         , done = \/-(None)
         ))

    M.tailrecM(proceed)(this)
  }

  def ::(a: => A)(implicit M: Applicative[M]): StreamT[M, A] = StreamT[M, A](M.point(Yield(a, this)))

  def isEmpty(implicit M: Monad[M]): M[Boolean] = M.map(uncons)(!_.isDefined)
  def isEmptyRec(implicit M: BindRec[M]): M[Boolean] = M.map(unconsRec)(!_.isDefined)

  def head(implicit M: Monad[M]): M[A] = M.map(uncons)(_.getOrElse(sys.error("head: empty StreamT"))._1)
  def headRec(implicit M: BindRec[M]): M[A] = M.map(unconsRec)(_.getOrElse(sys.error("head: empty StreamT"))._1)

  def headOption(implicit M: Monad[M]): M[Option[A]] = M.map(uncons)(_.map(_._1))
  def headOptionRec(implicit M: BindRec[M]): M[Option[A]] = M.map(unconsRec)(_.map(_._1))

  def tailM(implicit M: Monad[M]): M[StreamT[M, A]] = M.map(uncons)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)
  def tailMRec(implicit M: BindRec[M]): M[StreamT[M, A]] = M.map(unconsRec)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)

  def trans[N[_]](t: M ~> N)(implicit M: Functor[M], N: Functor[N]): StreamT[N, A] =
    StreamT(t(M.map(this.step)(
      _( yieldd = (a, as) => Yield(a, as trans t)
       , skip = as => Skip(as trans t)
       , done = Done
       ))))

  def filter(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
     , skip = as => Skip(as filter p)
     , done = Done
     )
  }

  def drop(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (n > 0) Skip(as drop (n-1)) else Yield(a, as)
     , skip = as => Skip(as drop n)
     , done = Done
     )
  }

  def dropWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) Skip(as dropWhile p) else Yield(a, as)
     , skip = as => Skip(as dropWhile p)
     , done = Done
     )
  }

  def take(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (n <= 0) Done else Yield(a, as take (n-1))
     , skip = as => Skip(as take n)
     , done = Done
     )
  }

  def takeWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => if (p(a)) Yield(a, as takeWhile p) else Done
     , skip = as => Skip(as takeWhile p)
     , done = Done
     )
  }

  def ++(bs: => StreamT[M, A])(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, as) => Yield(a, as ++ bs)
     , skip = as => Skip(as ++ bs)
     , done = Skip(bs)
     )
  }

  def flatMap[B](f: A => StreamT[M, B])(implicit m: Functor[M]): StreamT[M, B] = stepMap {
    _( yieldd = (a, s) => Skip(f(a) ++ (s flatMap f))
     , skip = s => Skip(s flatMap f)
     , done = Done
     )
  }

  def map[B](f: A => B)(implicit m: Functor[M]): StreamT[M, B] = stepMap {
    _( yieldd = (a, s) => Yield(f(a), s map f)
     , skip = s => Skip(s map f)
     , done = Done
     )
  }

  /** @since 7.0.1 */
  def mapM[B](f: A => M[B])(implicit m: Monad[M]): StreamT[M, B] = stepBind {
    _( yieldd = (a, s) => m.map(f(a)) { Yield(_, s mapM f) }
     , skip = s => m.point(Skip(s mapM f))
     , done = m.point(Done)
     )
  }

  /**Don't use iteratively! */
  def tail(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    _( yieldd = (a, s) => Skip(s)
     , skip = s => Skip(s.tail)
     , done = sys.error("tail: empty StreamT")
     )
  }

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      _( yieldd = (a, s) => s.foldLeft(f(z, a))(f)
       , skip = s => s.foldLeft(z)(f)
       , done = M.point(z)
       )
    }

  def foldLeftRec[B](z: B)(f: (B, A) => B)(implicit M: BindRec[M]): M[B] = {
    def proceed(sb: (StreamT[M, A], B)): M[(StreamT[M, A], B) \/ B] =
      M.map(sb._1.step) {
        _( yieldd = (a, s) => -\/((s, f(sb._2, a)))
         , skip = s => -\/((s, sb._2))
         , done = \/-(sb._2)
         )
      }

    M.tailrecM(proceed)((this, z))
  }

  /**
   * **Warning:** Requires evaluation of the whole stream. Depending on
   * the monad `M`, the evaluation will happen either immediately, or
   * will be deferred until the resulting `Stream` is extracted from the
   * returned `M`.
   */
  def toStream(implicit M: Monad[M]): M[Stream[A]] = M.map(rev)(_.reverse)

  /**
   * **Warning:** Requires evaluation of the whole stream. Depending on
   * the monad `M`, the evaluation will happen either immediately, or
   * will be deferred until the resulting `Stream` is extracted from the
   * returned `M`.
   */
  def toStreamRec(implicit M: BindRec[M]): M[Stream[A]] = M.map(revRec)(_.reverse)

  /**
   * Converts this `StreamT` to a lazy `Stream`, i.e. without forcing
   * evaluation of all elements. Note, however, that at least one element
   * of this stream will be evaluated, and depending on the structure of
   * this stream, up to two elements might be evaluated.
   */
  def asStream(implicit ev: M[Step[A, StreamT[M, A]]] =:= Id[Step[A, StreamT[Id, A]]]): Stream[A] = {
    def go(s: StreamT[Id, A]): Stream[A] = s.unconsRec match {
      case None => Stream.empty[A]
      case Some((a, s1)) => Stream.cons(a, go(s1))
    }

    go(StreamT(ev(step)))
  }

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Monad[M]): M[B] =
    M.map(rev) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  def foldRightRec[B](z: => B)(f: (=> A, => B) => B)(implicit M: BindRec[M]): M[B] =
    M.map(revRec) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  /**
   * `foldRight` with potential to terminate early, e.g. on an infinite stream.
   */
  def foldRightM[B](z: => M[B])(f: (=> A, => M[B]) => M[B])(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      _( yieldd = (a, s) => f(a, s.foldRightM(z)(f))
       , skip = s => s.foldRightM(z)(f)
       , done = z
       )
    }

  def foldMap[B](f: A => B)(implicit M: Foldable[M], B: Monoid[B]): B =
    M.foldMap(step) {
      _( yieldd = (a, s) => B.append(f(a), s.foldMap(f))
       , skip = s => s.foldMap(f)
       , done = B.zero
       )
    }

  def length(implicit m: Monad[M]): M[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }

  def lengthRec(implicit M: BindRec[M]): M[Int] =
    foldLeftRec(0)((c, a) => 1 + c)

  def foreach(f: A => M[Unit])(implicit M: Monad[M]): M[Unit] = M.bind(step) {
    case Yield(a,s) => M.bind(f(a))(_ => s().foreach(f))
    case Skip(s) => s().foreach(f)
    case Done => M.pure(())
  }

  def foreachRec(f: A => M[Unit])(implicit M: Monad[M], B: BindRec[M]): M[Unit] = {
    def proceed(s: StreamT[M, A]): M[StreamT[M, A] \/ Unit] = M.bind(s.step) {
      case Yield(a, s1) => M.map(f(a))(_ => -\/(s1()))
      case Skip(s1) => M.pure(-\/(s1()))
      case Done => M.pure(\/-(()))
    }

    B.tailrecM(proceed)(this)
  }

  private def stepMap[B](f: Step[A, StreamT[M, A]] => Step[B, StreamT[M, B]])(implicit M: Functor[M]): StreamT[M, B] = StreamT(M.map(step)(f))

  private def stepBind[B](f: Step[A, StreamT[M, A]] => M[Step[B, StreamT[M, B]]])(implicit M: Monad[M]): StreamT[M, B] = StreamT(M.bind(step)(f))

  private def rev(implicit M: Monad[M]): M[Stream[A]] = {
    def loop(xs: StreamT[M, A], ys: Stream[A]): M[Stream[A]] =
      M.bind(xs.step) {
        _( yieldd = (a, s) => loop(s, a #:: ys)
         , skip = s => loop(s, ys)
         , done = M.point(ys)
         )
      }
    loop(this, Stream.Empty)
  }

  private def revRec(implicit M: BindRec[M]): M[Stream[A]] = {
    def loop(ss: (StreamT[M, A], Stream[A])): M[(StreamT[M, A], Stream[A]) \/ Stream[A]] = {
      val (xs, ys) = ss
      M.map(xs.step) {
        _( yieldd = (a, s) => -\/((s, a #:: ys))
         , skip = s => -\/((s, ys))
         , done = \/-(ys)
         )
      }
    }

    M.tailrecM(loop)((this, Stream.Empty))
  }
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class StreamTInstances0 {
  implicit def StreamTInstance1[F[_]](implicit F0: Functor[F]): Bind[StreamT[F, ?]] with Plus[StreamT[F, ?]] =
    new StreamTInstance1[F] {
      implicit def F: Functor[F] = F0
    }

  implicit def StreamTSemigroup[F[_], A](implicit F0: Functor[F]): Semigroup[StreamT[F, A]] =
    new StreamTSemigroup[F, A] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class StreamTInstances extends StreamTInstances0 {
  implicit def StreamTMonoid[F[_], A](implicit F0: Applicative[F]): Monoid[StreamT[F, A]] =
    new StreamTMonoid[F, A] {
      implicit def F: Applicative[F] = F0
    }
  implicit def StreamTMonadPlus[F[_]](implicit F0: Applicative[F]): MonadPlus[StreamT[F, ?]] =
    new StreamTMonadPlus[F] {
      implicit def F: Applicative[F] = F0
    }
  implicit def StreamTEqual[F[_], A](implicit E: Equal[F[Stream[A]]], F: Monad[F]): Equal[StreamT[F, A]] = E.contramap((_: StreamT[F, A]).toStream)
  implicit def StreamTShow[F[_], A](implicit E: Show[F[Stream[A]]], F: Monad[F]): Show[StreamT[F, A]] = Contravariant[Show].contramap(E)((_: StreamT[F, A]).toStream)
  implicit val StreamTHoist: Hoist[StreamT] = new StreamTHoist {}
  implicit def StreamTFoldable[F[_]: Foldable]: Foldable[StreamT[F, ?]] =
    new Foldable[StreamT[F, ?]] with Foldable.FromFoldMap[StreamT[F, ?]] {
      override def foldMap[A, M: Monoid](s: StreamT[F, A])(f: A => M) = s.foldMap(f)
    }
}

object StreamT extends StreamTInstances {
  def apply[M[_], A](step: M[Step[A, StreamT[M, A]]]): StreamT[M, A] = new StreamT[M, A](step)

  def empty[M[_], A](implicit M: Applicative[M]): StreamT[M, A] = new StreamT[M, A](M point Done)

  def fromStream[M[_], A](mas: M[Stream[A]])(implicit M: Applicative[M]): StreamT[M, A] = {
    def loop(as: Stream[A]): Step[A, StreamT[M, A]] = as match {
      case head #:: tail => Yield(head, apply(M.point(loop(tail))))
      case _ => Done
    }

    apply[M, A](M.map(mas)(loop))
  }

  def unfoldM[M[_],A,B](start: B)(f: B => M[Option[(A,B)]])(implicit M: Functor[M]): StreamT[M,A] =
    StreamT[M,A](M.map(f(start)) {
      case Some((a, b)) => Yield(a, unfoldM(b)(f))
      case None => Done
    })

  def unfold[A,B](b: B)(f: B => Option[(A,B)]): StreamT[Id,A] = unfoldM[Id,A,B](b)(f)

  def fromIterable[A](s: Iterable[A]): StreamT[Id,A] = {
    def stepper(b: Iterable[A]): Option[(A,Iterable[A])] = if (b.isEmpty) None else Some((b.head, b.tail))
    unfold(s)(stepper)
  }

  def wrapEffect[M[_]:Functor,A](m: M[StreamT[M,A]]): StreamT[M,A] = StreamT(Functor[M].map(m)(Skip(_)))

  abstract sealed class Step[+A, +S] {
    def apply[Z](yieldd: (A, => S) => Z, skip: => S => Z, done: => Z): Z
  }

  def runStreamT[S,A](stream : StreamT[State[S, ?],A], s0: S): StreamT[Id,A] =
    StreamT[Id,A]({
      val (s1, sa) = stream.step(s0)
      sa((a, as) => Yield(a, runStreamT(as, s1)),
         as => Skip(runStreamT(as, s1)),
         Done)
    })

  object Yield {
    def apply[A, S](a: A, s: => S): Step[A, S] =
      new Step[A, S] {
        def apply[Z](yieldd: (A, => S) => Z, skip: => S => Z, done: => Z) = yieldd(a, s)
      }
    def unapply[A, S](s: Step[A, S]): Option[(A, () => S)] =
      s((aa, sa) => Some((aa, () => sa)), _ => None, None)
  }

  object Skip {
    def apply[S](s: => S): Step[Nothing, S] =
      new Step[Nothing, S] {
        def apply[Z](yieldd: (Nothing, => S) => Z, skip: => S => Z, done: => Z) = skip(s)
      }
    def unapply[A, S](s: Step[A, S]): Option[(() => S)] =
      s((_, _) => None, s => Some(() => s), None)
  }

  object Done extends Step[Nothing, Nothing] {
    def apply[Z](yieldd: (Nothing, => Nothing) => Z, skip: => Nothing => Z, done: => Z) = done
    def unapply[A, S](s: Step[A, S]): Boolean = s((_, _) => false, _ => false, true)
  }
}

//
// Implementation traits for type class instances
//

private trait StreamTInstance1[F[_]] extends Bind[StreamT[F, ?]] with Plus[StreamT[F, ?]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: StreamT[F, A])(f: A => B) =
    fa map f

  override final def bind[A, B](fa: StreamT[F, A])(f: A => StreamT[F, B]) =
    fa flatMap f

  override final def plus[A](a: StreamT[F, A], b: => StreamT[F, A]) =
    a ++ b
}

private trait StreamTSemigroup[F[_], A] extends Semigroup[StreamT[F, A]] {
  implicit def F: Functor[F]

  def append(f1: StreamT[F, A], f2: => StreamT[F, A]): StreamT[F, A] = f1 ++ f2
}

private trait StreamTMonoid[F[_], A] extends Monoid[StreamT[F, A]] with StreamTSemigroup[F, A] {
  implicit def F: Applicative[F]

  def zero: StreamT[F, A] = StreamT.empty[F, A]
}

private trait StreamTMonadPlus[F[_]] extends MonadPlus[StreamT[F, ?]] with StreamTInstance1[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): StreamT[F, A] = a :: StreamT.empty[F, A]

  def empty[A]: StreamT[F, A] = StreamT.empty
}

private trait StreamTHoist extends Hoist[StreamT] {
  import StreamT._

  implicit def apply[G[_] : Monad]: Monad[StreamT[G, ?]] = StreamTMonadPlus[G]

  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): StreamT[G, A] = StreamT[G, A](G.map(a)(Yield(_, empty)))

  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): StreamT[M, ?] ~> StreamT[N, ?] =
    λ[StreamT[M, ?] ~> StreamT[N, ?]](a =>
      StreamT(f(M.map(a.step)(
        _( yieldd = (a, as) => Yield(a, hoist(f) apply as)
         , skip = as => Skip(hoist(f) apply as)
         , done = Done
         ))))
    )
}
