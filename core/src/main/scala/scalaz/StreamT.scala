package scalaz

import Id._

/**
 * StreamT monad transformer.
 */
sealed class StreamT[M[_], A](val step: M[StreamT.Step[A, StreamT[M, A]]]) {

  import StreamT._

  def uncons(implicit M: Monad[M]): M[Option[(A, StreamT[M, A])]] =
    M.bind(step) {
      case Yield(a, s) => M.point(Some((a, s())))
      case Skip(s)     => s().uncons
      case Done()      => M.point(None)
    }

  def unconsRec(implicit M: BindRec[M]): M[Option[(A, StreamT[M, A])]] =
    M.tailrecM(this)(s =>
      M.map(s.step) {
        case Yield(a, s1) => \/-(Some((a, s1())))
        case Skip(s1)     => -\/(s1())
        case Done()       => \/-(None)
      })

  def ::(a: A)(implicit M: Applicative[M]): StreamT[M, A] = StreamT[M, A](M.point(Yield(a, this)))

  def isEmpty(implicit M: Monad[M]): M[Boolean] = M.map(uncons)(_.isEmpty)
  def isEmptyRec(implicit M: BindRec[M]): M[Boolean] = M.map(unconsRec)(_.isEmpty)

  def head(implicit M: Monad[M]): M[A] = M.map(uncons)(_.getOrElse(sys.error("head: empty StreamT"))._1)
  def headRec(implicit M: BindRec[M]): M[A] = M.map(unconsRec)(_.getOrElse(sys.error("head: empty StreamT"))._1)

  def headOption(implicit M: Monad[M]): M[Option[A]] = M.map(uncons)(_.map(_._1))
  def headOptionRec(implicit M: BindRec[M]): M[Option[A]] = M.map(unconsRec)(_.map(_._1))

  def tailM(implicit M: Monad[M]): M[StreamT[M, A]] = M.map(uncons)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)
  def tailMRec(implicit M: BindRec[M]): M[StreamT[M, A]] = M.map(unconsRec)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)

  def tailOption(implicit M: Monad[M]): M[Option[StreamT[M, A]]] =
    M.map(uncons)(_.map(_._2))
  def tailOptionRec(implicit M: BindRec[M]): M[Option[StreamT[M, A]]] =
    M.map(unconsRec)(_.map(_._2))

  def trans[N[_]](t: M ~> N)(implicit M: Functor[M], N: Functor[N]): StreamT[N, A] =
    StreamT(t(M.map[Step[A, StreamT[M, A]], Step[A, StreamT[N, A]]](this.step) {
      case Yield(a, s) => Yield(a, s() trans t)
      case Skip(s)     => Skip(s() trans t)
      case Done()      => Done()
    }))

  def filter(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (p(a)) Yield(a, s() filter p) else Skip(s() filter p)
    case Skip(s)     => Skip(s() filter p)
    case Done()      => Done()
  }

  def drop(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (n > 0) Skip(s() drop (n - 1)) else Yield(a, s)
    case Skip(s)     => Skip(s() drop n)
    case Done()      => Done()
  }

  def dropWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (p(a)) Skip(s() dropWhile p) else Yield(a, s)
    case Skip(s)     => Skip(s() dropWhile p)
    case Done()      => Done()
  }

  def take(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (n <= 0) Done() else Yield(a, s() take (n - 1))
    case Skip(s)     => Skip(s() take n)
    case Done()      => Done()
  }

  def takeWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (p(a)) Yield(a, s() takeWhile p) else Done()
    case Skip(s)     => Skip(s() takeWhile p)
    case Done()      => Done()
  }

  def ++(bs: => StreamT[M, A])(implicit m: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => Yield(a, s() ++ bs)
    case Skip(s)     => Skip(s() ++ bs)
    case Done()      => Skip(bs)
  }

  def flatMap[B](f: A => StreamT[M, B])(implicit m: Functor[M]): StreamT[M, B] = stepMap[B] {
    case Yield(a, s) => Skip(f(a) ++ (s() flatMap f))
    case Skip(s)     => Skip(s() flatMap f)
    case Done()      => Done()
  }

  def map[B](f: A => B)(implicit m: Functor[M]): StreamT[M, B] = stepMap[B] {
    case Yield(a, s) => Yield(f(a), s() map f)
    case Skip(s)     => Skip(s() map f)
    case Done()      => Done()
  }

  /** @since 7.0.1 */
  def mapM[B](f: A => M[B])(implicit M: Monad[M]): StreamT[M, B] = stepBind[B] {
    case Yield(a, s) => M.map(f(a))(Yield(_, s() mapM f))
    case Skip(s)     => M.point(Skip(s() mapM f))
    case Done()      => M.point(Done())
  }

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      case Yield(a, s) => s().foldLeft(f(z, a))(f)
      case Skip(s)     => s().foldLeft(z)(f)
      case Done()      => M.point(z)
    }

  def foldLeftRec[B](z: B)(f: (B, A) => B)(implicit M: BindRec[M]): M[B] =
    M.tailrecM((() => this, z))(sb => M.map(sb._1().step) {
      case Yield(a, s) => -\/((s, f(sb._2, a)))
      case Skip(s)     => -\/((s, sb._2))
      case Done()      => \/-(sb._2)
    })

  /**
   * **Warning:** Requires evaluation of the whole stream. Depending on
   * the monad `M`, the evaluation will happen either immediately, or
   * will be deferred until the resulting `LazyList` is extracted from the
   * returned `M`.
   */
  def toLazyList(implicit M: Monad[M]): M[LazyList[A]] = M.map(rev)(_.reverse)

  /**
   * **Warning:** Requires evaluation of the whole stream. Depending on
   * the monad `M`, the evaluation will happen either immediately, or
   * will be deferred until the resulting `LazyList` is extracted from the
   * returned `M`.
   */
  def toLazyListRec(implicit M: BindRec[M]): M[LazyList[A]] = M.map(revRec)(_.reverse)

  /**
   * Converts this `StreamT` to a lazy `LazyList`, i.e. without forcing
   * evaluation of all elements. Note, however, that at least one element
   * of this stream will be evaluated, and depending on the structure of
   * this stream, up to two elements might be evaluated.
   */
  def asLazyList(implicit ev: M[Step[A, StreamT[M, A]]] === Id[Step[A, StreamT[Id, A]]]): LazyList[A] = {
    def go(s: StreamT[Id, A]): LazyList[A] = s.unconsRec match {
      case None          => LazyList.empty[A]
      case Some((a, s1)) => LazyList.cons(a, go(s1))
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
      case Yield(a, s) => f(a, s().foldRightM(z)(f))
      case Skip(s)     => s().foldRightM(z)(f)
      case Done()      => z
    }

  def foldMap[B](f: A => B)(implicit M: Foldable[M], B: Monoid[B]): B =
    M.foldMap(step) {
      case Yield(a, s) => B.append(f(a), s().foldMap(f))
      case Skip(s)     => s().foldMap(f)
      case Done()      => B.zero
    }

  def length(implicit m: Monad[M]): M[Int] =
    foldLeft(0)((c, a) => 1 + c)

  def lengthRec(implicit M: BindRec[M]): M[Int] =
    foldLeftRec(0)((c, a) => 1 + c)

  def foreach(f: A => M[Unit])(implicit M: Monad[M]): M[Unit] = M.bind(step) {
    case Yield(a,s) => M.bind(f(a))(_ => s() foreach f)
    case Skip(s)    => s().foreach(f)
    case Done()     => M.pure(())
  }

  def foreachRec(f: A => M[Unit])(implicit M: Monad[M], B: BindRec[M]): M[Unit] =
    B.tailrecM(() => this)(s => M.bind(s().step) {
      case Yield(a, s1) => M.map(f(a))(_ => -\/(s1))
      case Skip(s1)     => M.pure(-\/(s1))
      case Done()       => M.pure(\/-(()))
    })

  private def stepMap[B](f: Step[A, StreamT[M, A]] => Step[B, StreamT[M, B]])(implicit M: Functor[M]): StreamT[M, B] = StreamT(M.map(step)(f))

  private def stepBind[B](f: Step[A, StreamT[M, A]] => M[Step[B, StreamT[M, B]]])(implicit M: Monad[M]): StreamT[M, B] = StreamT(M.bind(step)(f))

  private def rev(implicit M: Monad[M]): M[LazyList[A]] = {
    def loop(xs: StreamT[M, A], ys: LazyList[A]): M[LazyList[A]] =
      M.bind(xs.step) {
        case Yield(a, s) => loop(s(), a #:: ys)
        case Skip(s)     => loop(s(), ys)
        case Done()      => M.point(ys)
      }
    loop(this, LazyList.empty)
  }

  private def revRec(implicit M: BindRec[M]): M[LazyList[A]] =
    M.tailrecM((() => this, LazyList.empty[A])) { case (xs, ys) =>
      M.map(xs().step) {
        case Yield(a, s) => -\/((s, a #:: ys))
        case Skip(s)     => -\/((s, ys))
        case Done()      => \/-(ys)
      }
    }

}

//
// Prioritized Implicits for type class instances
//

sealed abstract class StreamTInstances0 {
  implicit def StreamTInstance1[F[_]](implicit F0: Functor[F]): Bind[StreamT[F, *]] with Plus[StreamT[F, *]] =
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
  implicit def StreamTMonadPlus[F[_]](implicit F0: Applicative[F]): MonadPlus[StreamT[F, *]] =
    new StreamTMonadPlus[F] {
      implicit def F: Applicative[F] = F0
    }
  implicit def StreamTEqual[F[_], A](implicit E: Equal[F[LazyList[A]]], F: Monad[F]): Equal[StreamT[F, A]] = E.contramap((_: StreamT[F, A]).toLazyList)
  implicit def StreamTShow[F[_], A](implicit E: Show[F[LazyList[A]]], F: Monad[F]): Show[StreamT[F, A]] = Contravariant[Show].contramap(E)((_: StreamT[F, A]).toLazyList)
  implicit val StreamTHoist: Hoist[StreamT] = new StreamTHoist {}
  implicit def StreamTFoldable[F[_]: Foldable]: Foldable[StreamT[F, *]] =
    new Foldable[StreamT[F, *]] with Foldable.FromFoldMap[StreamT[F, *]] {
      override def foldMap[A, M: Monoid](s: StreamT[F, A])(f: A => M) = s.foldMap(f)
    }
}

object StreamT extends StreamTInstances {
  def apply[M[_], A](step: M[Step[A, StreamT[M, A]]]): StreamT[M, A] = new StreamT[M, A](step)

  def empty[M[_], A](implicit M: Applicative[M]): StreamT[M, A] = new StreamT[M, A](M point Done())

  def fromLazyList[M[_], A](mas: M[LazyList[A]])(implicit M: Applicative[M]): StreamT[M, A] = {
    def loop(as: LazyList[A]): Step[A, StreamT[M, A]] = as match {
      case head #:: tail => Yield(head, apply(M.point(loop(tail))))
      case _ => Done()
    }

    apply[M, A](M.map(mas)(loop))
  }

  def unfoldM[M[_],A,B](start: B)(f: B => M[Option[(A,B)]])(implicit M: Functor[M]): StreamT[M,A] =
    StreamT[M,A](M.map(f(start)) {
      case Some((a, b)) => Yield(a, unfoldM(b)(f))
      case None => Done()
    })

  def unfold[A,B](b: B)(f: B => Option[(A,B)]): StreamT[Id,A] = unfoldM[Id,A,B](b)(f)

  def fromIterable[A](s: Iterable[A]): StreamT[Id,A] = {
    def stepper(b: Iterable[A]): Option[(A,Iterable[A])] = if (b.isEmpty) None else Some((b.head, b.tail))
    unfold(s)(stepper)
  }

  def wrapEffect[M[_]:Functor,A](m: M[StreamT[M,A]]): StreamT[M,A] = StreamT(Functor[M].map(m)(Skip(_)))

  def runStreamT[S,A](stream : StreamT[State[S, *],A], s0: S): StreamT[Id,A] =
    StreamT[Id,A]({
      val (a, s) = stream.step(s0)
      s match {
        case Yield(a1, s1) => Yield(a1, runStreamT(s1(), a))
        case Skip(s1)      => Skip(runStreamT(s1(), a))
        case Done()        => Done()
      }
    })


  sealed abstract class Step[A, S] extends Product with Serializable

  final case class Yield[A, S](a: A, s: () => S) extends Step[A, S]
  object Yield {
    def apply[A, S](a: A, s: => S): Step[A, S] = new Yield(a, () => s)
  }
  final case class Skip[A, S](s: () => S) extends Step[A, S]
  object Skip {
    def apply[A, S](s: => S): Step[A, S] = new Skip(() => s)
  }
  sealed abstract case class Done[A, S]() extends Step[A, S]
  object Done {
    def apply[A, S](): Step[A, S] = done_.asInstanceOf[Done[A, S]]
    // https://github.com/scala/bug/issues/11953
    private[this] final val done_ : Done[Nothing, Nothing] = new Done[Nothing, Nothing]{}
  }
}

//
// Implementation traits for type class instances
//

private trait StreamTInstance1[F[_]] extends Bind[StreamT[F, *]] with Plus[StreamT[F, *]] {
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

private trait StreamTMonadPlus[F[_]] extends MonadPlus[StreamT[F, *]] with StreamTInstance1[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): StreamT[F, A] = a :: StreamT.empty[F, A]

  def empty[A]: StreamT[F, A] = StreamT.empty
}

private trait StreamTHoist extends Hoist[StreamT] {
  import StreamT._

  implicit def apply[G[_] : Monad]: Monad[StreamT[G, *]] = StreamTMonadPlus[G]

  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): StreamT[G, A] =
    StreamT[G, A](G.map(a)(Yield(_, empty[G, A](G))))

  override def wrapEffect[G[_]: Monad, A](a: G[StreamT[G, A]]): StreamT[G, A] = StreamT.wrapEffect(a)

  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): StreamT[M, *] ~> StreamT[N, *] =
    λ[StreamT[M, *] ~> StreamT[N, *]](a =>
      StreamT(f(M.map(a.step) {
        case Yield(a, s) => Yield(a, hoist(f).apply(s()))
        case Skip(s)     => Skip(hoist(f).apply(s()))
        case Done()      => Done()
      }
    )))
}
