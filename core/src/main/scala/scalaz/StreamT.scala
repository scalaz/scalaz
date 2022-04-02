package scalaz

import Id._

/** StreamT monad transformer.
  */
abstract class StreamT[M[_], A] { self =>

  import StreamT._

  def step: M[StreamT.Step[M, A]]

  /** Returns a new [[StreamT]] that does not contain [[StreamT.Skip]] steps,
    * while yields the same elements as `this`.
    */
  def noSkip(implicit M: Monad[M]): StreamT[M, A] = new StreamT[M, A] {
    def step = M.bind(self.step) {
      case Yield(a, s) =>
        M.point(Yield(a, s.noSkip))
      case Skip(s) =>
        s.noSkip.step
      case Done() =>
        M.point(Done())
    }
  }

  /** Returns a new [[StreamT]] that does not contain [[StreamT.Skip]] steps,
    * while yields the same elements as `this`.
    *
    * @note
    *   Unlike [[noSkip]], this [[noSkipRec]] is stack safe.
    */
  def noSkipRec(implicit M: BindRec[M]): StreamT[M, A] = new StreamT[M, A] {
    def step =
      M.tailrecM(self) { s =>
        M.map(s.step) {
          case Yield(a, s1) => \/-(Yield(a, s1.noSkipRec))
          case Skip(s1)     => -\/(s1)
          case Done()       => \/-(Done())
        }
      }
  }

  def uncons(implicit M: Monad[M]): M[Option[(A, StreamT[M, A])]] =
    M.bind(step) {
      case Yield(a, s) => M.point(Some((a, s)))
      case Skip(s)     => s.uncons
      case Done()      => M.point(None)
    }

  def unconsRec(implicit M: BindRec[M]): M[Option[(A, StreamT[M, A])]] =
    M.tailrecM(this)(s =>
      M.map(s.step) {
        case Yield(a, s1) => \/-(Some((a, s1)))
        case Skip(s1)     => -\/(s1)
        case Done()       => \/-(None)
      }
    )

  def ::(a: A)(implicit M: Applicative[M]): StreamT[M, A] =
    StreamT[M, A](M.point(Yield(a, this)))

  def isEmpty(implicit M: Monad[M]): M[Boolean] = M.map(uncons)(_.isEmpty)
  def isEmptyRec(implicit M: BindRec[M]): M[Boolean] =
    M.map(unconsRec)(_.isEmpty)

  def head(implicit M: Monad[M]): M[A] =
    M.map(uncons)(_.getOrElse(sys.error("head: empty StreamT"))._1)
  def headRec(implicit M: BindRec[M]): M[A] =
    M.map(unconsRec)(_.getOrElse(sys.error("head: empty StreamT"))._1)

  def headOption(implicit M: Monad[M]): M[Option[A]] =
    M.map(uncons)(_.map(_._1))
  def headOptionRec(implicit M: BindRec[M]): M[Option[A]] =
    M.map(unconsRec)(_.map(_._1))

  def tailM(implicit M: Monad[M]): M[StreamT[M, A]] =
    M.map(uncons)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)
  def tailMRec(implicit M: BindRec[M]): M[StreamT[M, A]] =
    M.map(unconsRec)(_.getOrElse(sys.error("tailM: empty StreamT"))._2)

  def tailOption(implicit M: Monad[M]): M[Option[StreamT[M, A]]] =
    M.map(uncons)(_.map(_._2))
  def tailOptionRec(implicit M: BindRec[M]): M[Option[StreamT[M, A]]] =
    M.map(unconsRec)(_.map(_._2))

  def trans[N[_]](t: M ~> N)(implicit M: Functor[M]): StreamT[N, A] =
    StreamT(t(M.map[Step[M, A], Step[N, A]](this.step) {
      case Yield(a, s) => Yield(a, s trans t)
      case Skip(s)     => Skip(s trans t)
      case Done()      => Done()
    }))

  def filter(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] =
    stepMap[A] {
      case Yield(a, s) => if (p(a)) Yield(a, s filter p) else Skip(s filter p)
      case Skip(s)     => Skip(s filter p)
      case Done()      => Done()
    }

  def collect[B](
      pf: PartialFunction[A, B]
  )(implicit M: Functor[M]): StreamT[M, B] =
    StreamT(M.map(step) {
      case Yield(pf(b), s) =>
        Yield(b, s.collect(pf))
      case Yield(_, s) =>
        Skip(s.collect(pf))
      case Skip(s) =>
        Skip(s.collect(pf))
      case Done() =>
        Done()
    })

  def drop(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (n > 0) Skip(s drop (n - 1)) else Yield(a, s)
    case Skip(s)     => Skip(s drop n)
    case Done()      => Done()
  }

  def dropWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] =
    stepMap[A] {
      case Yield(a, s) => if (p(a)) Skip(s dropWhile p) else Yield(a, s)
      case Skip(s)     => Skip(s dropWhile p)
      case Done()      => Done()
    }

  def postScanLeft[B](
      head: B
  )(op: (B, A) => B)(implicit M: Functor[M]): StreamT[M, B] =
    stepMap[B] {
      case Yield(a, s) =>
        val next = op(head, a)
        Yield(next, s.postScanLeft(next)(op))
      case Skip(s) =>
        Skip(s.postScanLeft(head)(op))
      case Done() =>
        Done()
    }

  /** Produces a [[StreamT]] containing cumulative results of applying the
    * operator going left to right, including the initial value.
    *
    * @tparam B
    *   the type of the elements in the resulting collection
    * @param head
    *   the initial value
    * @param op
    *   the binary operator applied to the intermediate result and the element
    * @return
    *   collection with intermediate results
    */
  def scanLeft[B](head: B)(op: (B, A) => B)(implicit
      M: Applicative[M]
  ): StreamT[M, B] =
    head :: postScanLeft(head)(op)

  /** Computes a prefix scan of the elements of the collection.
    *
    * Note: The neutral element `B` may be applied more than once.
    *
    * @tparam B
    *   element type of the resulting collection
    * @param op
    *   the associative operator for the scan
    *
    * @return
    *   a new [[StreamT]] containing the prefix scan of the elements in this
    *   $coll
    */
  def scan[B >: A](op: (B, B) => B)(implicit M: Functor[M]): StreamT[M, B] =
    stepMap[B] {
      case Yield(a, s) =>
        Yield(a, s.postScanLeft[B](a)(op))
      case Skip(s) =>
        Skip(s.scan(op))
      case Done() =>
        Done()
    }

  def take(n: Int)(implicit M: Functor[M]): StreamT[M, A] = stepMap[A] {
    case Yield(a, s) => if (n <= 0) Done() else Yield(a, s take (n - 1))
    case Skip(s)     => Skip(s take n)
    case Done()      => Done()
  }

  def takeWhile(p: A => Boolean)(implicit m: Functor[M]): StreamT[M, A] =
    stepMap[A] {
      case Yield(a, s) => if (p(a)) Yield(a, s takeWhile p) else Done()
      case Skip(s)     => Skip(s takeWhile p)
      case Done()      => Done()
    }

  def ++(bs: => StreamT[M, A])(implicit m: Functor[M]): StreamT[M, A] =
    stepMap[A] {
      case Yield(a, s) => Yield(a, s ++ bs)
      case Skip(s)     => Skip(s ++ bs)
      case Done()      => Skip(bs)
    }

  def flatMap[B](f: A => StreamT[M, B])(implicit m: Functor[M]): StreamT[M, B] =
    stepMap[B] {
      case Yield(a, s) => Skip(f(a) ++ (s flatMap f))
      case Skip(s)     => Skip(s flatMap f)
      case Done()      => Done()
    }

  def map[B](f: A => B)(implicit m: Functor[M]): StreamT[M, B] = stepMap[B] {
    case Yield(a, s) => Yield(f(a), s map f)
    case Skip(s)     => Skip(s map f)
    case Done()      => Done()
  }

  /** @since 7.0.1 */
  def mapM[B](f: A => M[B])(implicit M: Monad[M]): StreamT[M, B] = stepBind[B] {
    case Yield(a, s) => M.map(f(a))(Yield(_, s mapM f))
    case Skip(s)     => M.point(Skip(s mapM f))
    case Done()      => M.point(Done())
  }

  def weakMemoize(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    case Yield(a, s) =>
      val memo = EphemeralStream.weakMemo(s.step)
      Yield(
        a,
        new StreamT[M, A] {
          val step = memo()
        }
      )
    case Skip(s) =>
      val memo = EphemeralStream.weakMemo(s.step)
      Skip(new StreamT[M, A] {
        val step = memo()
      })
    case Done() => Done()
  }

  def memoize(implicit m: Functor[M]): StreamT[M, A] = stepMap {
    case Yield(a, s) =>
      Yield(
        a,
        new StreamT[M, A] {
          lazy val step = s.step
        }
      )
    case Skip(s) =>
      Skip(new StreamT[M, A] {
        lazy val step = s.step
      })
    case Done() => Done()
  }

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      case Yield(a, s) => s.foldLeft(f(z, a))(f)
      case Skip(s)     => s.foldLeft(z)(f)
      case Done()      => M.point(z)
    }

  def foldLeftRec[B](z: B)(f: (B, A) => B)(implicit M: BindRec[M]): M[B] =
    M.tailrecM((this, z))(sb =>
      M.map(sb._1.step) {
        case Yield(a, s) => -\/((s, f(sb._2, a)))
        case Skip(s)     => -\/((s, sb._2))
        case Done()      => \/-(sb._2)
      }
    )

  /** **Warning:** Requires evaluation of the whole stream. Depending on the
    * monad `M`, the evaluation will happen either immediately, or will be
    * deferred until the resulting `LazyList` is extracted from the returned
    * `M`.
    */
  def toLazyList(implicit M: Monad[M]): M[LazyList[A]] = M.map(rev)(_.reverse)

  /** **Warning:** Requires evaluation of the whole stream. Depending on the
    * monad `M`, the evaluation will happen either immediately, or will be
    * deferred until the resulting `LazyList` is extracted from the returned
    * `M`.
    */
  def toLazyListRec(implicit M: BindRec[M]): M[LazyList[A]] =
    M.map(revRec)(_.reverse)

  /** Converts this `StreamT` to a lazy `LazyList`, i.e. without forcing
    * evaluation of all elements. Note, however, that at least one element of
    * this stream will be evaluated, and depending on the structure of this
    * stream, up to two elements might be evaluated.
    */
  def asLazyList(implicit
      ev: M[Step[M, A]] === Id[Step[Id, A]]
  ): LazyList[A] = {
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

  def foldRightRec[B](
      z: => B
  )(f: (=> A, => B) => B)(implicit M: BindRec[M]): M[B] =
    M.map(revRec) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  /** `foldRight` with potential to terminate early, e.g. on an infinite stream.
    */
  def foldRightM[B](
      z: => M[B]
  )(f: (=> A, => M[B]) => M[B])(implicit M: Monad[M]): M[B] =
    M.bind(step) {
      case Yield(a, s) => f(a, s.foldRightM(z)(f))
      case Skip(s)     => s.foldRightM(z)(f)
      case Done()      => z
    }

  def foldMap[B](f: A => B)(implicit M: Foldable[M], B: Monoid[B]): B =
    M.foldMap(step) {
      case Yield(a, s) => B.append(f(a), s.foldMap(f))
      case Skip(s)     => s.foldMap(f)
      case Done()      => B.zero
    }

  def length(implicit m: Monad[M]): M[Int] =
    foldLeft(0)((c, a) => 1 + c)

  def lengthRec(implicit M: BindRec[M]): M[Int] =
    foldLeftRec(0)((c, a) => 1 + c)

  def foreach(f: A => M[Unit])(implicit M: Monad[M]): M[Unit] = M.bind(step) {
    case Yield(a, s) => M.bind(f(a))(_ => s foreach f)
    case Skip(s)     => s.foreach(f)
    case Done()      => M.pure(())
  }

  def foreachRec(
      f: A => M[Unit]
  )(implicit M: Monad[M], B: BindRec[M]): M[Unit] =
    B.tailrecM(this)(s =>
      M.bind(s.step) {
        case Yield(a, s1) => M.map(f(a))(_ => -\/(s1))
        case Skip(s1)     => M.pure(-\/(s1))
        case Done()       => M.pure(\/-(()))
      }
    )

  private def stepMap[B](f: Step[M, A] => Step[M, B])(implicit
      M: Functor[M]
  ): StreamT[M, B] = StreamT(M.map(step)(f))

  private def stepBind[B](f: Step[M, A] => M[Step[M, B]])(implicit
      M: Monad[M]
  ): StreamT[M, B] = StreamT(M.bind(step)(f))

  private def rev(implicit M: Monad[M]): M[LazyList[A]] = {
    def loop(xs: StreamT[M, A], ys: LazyList[A]): M[LazyList[A]] =
      M.bind(xs.step) {
        case Yield(a, s) => loop(s, a #:: ys)
        case Skip(s)     => loop(s, ys)
        case Done()      => M.point(ys)
      }
    loop(this, LazyList.empty)
  }

  private def revRec(implicit M: BindRec[M]): M[LazyList[A]] =
    M.tailrecM((this, LazyList.empty[A])) { case (xs, ys) =>
      M.map(xs.step) {
        case Yield(a, s) => -\/((s, a #:: ys))
        case Skip(s)     => -\/((s, ys))
        case Done()      => \/-(ys)
      }
    }

  /** Returns a new [[StreamT]] only containing items that are different from
    * their previous items.
    *
    * @param `latest`
    *   the assuming previous item of the head element of this [[StreamT]]
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/distinctUntilChanged.png"
    * />
    */
  def distinctUntilChanged(
      latest: A
  )(implicit M: Functor[M], A: Equal[A]): StreamT[M, A] =
    stepMap {
      case Skip(next) =>
        Skip(next.distinctUntilChanged(latest))
      case Done() =>
        Done()
      case Yield(a, next) if A.equal(a, latest) =>
        Skip(next.distinctUntilChanged(latest))
      case Yield(a, next) =>
        Yield(a, next.distinctUntilChanged(a))
    }

  /** Returns a new [[StreamT]] only containing items that are different from
    * their previous items.
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/distinctUntilChanged.png"
    * />
    */
  def distinctUntilChanged(implicit M: Functor[M], A: Equal[A]): StreamT[M, A] =
    stepMap {
      case Skip(next) =>
        Skip(next.distinctUntilChanged)
      case Done() =>
        Done()
      case Yield(a, next) =>
        Yield(a, next.distinctUntilChanged(a))
    }

  def mergeWith(
      f2: => StreamT[M, A]
  )(implicit M: Nondeterminism[M]): StreamT[M, A] = {
    StreamT(mergeFStep(this.step, f2.step))
  }

  def mergeMap[B](
      f: A => StreamT[M, B]
  )(implicit M: Nondeterminism[M]): StreamT[M, B] = {
    def mergeMapInitStep(fsa: M[Step[M, A]]): M[Step[M, B]] = {
      M.map(fsa) {
        case Yield(a, s) =>
          Skip(StreamT(mergeMapStep(s.step, f(a).step)))
        case Skip(s) =>
          Skip(StreamT(mergeMapInitStep(s.step)))
        case Done() =>
          Done()
      }
    }
    def mergeMapStep(fsa: M[Step[M, A]], fsb: M[Step[M, B]]): M[Step[M, B]] = {
      M.map(M.choose(fsa, fsb)) {
        case -\/((sa, fsb)) =>
          sa match {
            case Yield(a, s) =>
              Skip(StreamT(mergeMapStep(s.step, mergeFStep(f(a).step, fsb))))
            case Skip(s) =>
              Skip(StreamT(mergeMapStep(s.step, fsb)))
            case Done() =>
              Skip(StreamT(fsb))
          }
        case \/-((fsa, sb)) =>
          sb match {
            case Yield(b, s) =>
              Yield(b, StreamT(mergeMapStep(fsa, s.step)))
            case Skip(s) =>
              Skip(StreamT(mergeMapStep(fsa, s.step)))
            case Done() =>
              Skip(StreamT(mergeMapInitStep(fsa)))
          }
      }
    }
    StreamT(mergeMapInitStep(this.step))
  }

  /** The [[flatMapLatest]] operator behaves much like the [[mergeMap]] except
    * that whenever a new item is emitted by the source [[StreamT]], it will not
    * subscribe to and stop mirroring the [[StreamT]] that was generated from
    * the previously-emitted item, and begin only mirroring the current one.
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/flatMapLatest.png"/>
    */
  def flatMapLatest[B](
      f: A => StreamT[M, B]
  )(implicit M: Nondeterminism[M]): StreamT[M, B] = {
    def flatMapLatestInitStep(fsa: M[Step[M, A]]): M[Step[M, B]] = {
      M.map(fsa) {
        case Yield(a, s) =>
          Skip(StreamT(flatMapLatestStep(s.step, f(a).step)))
        case Skip(s) =>
          Skip(StreamT(flatMapLatestInitStep(s.step)))
        case Done() =>
          Done()
      }
    }
    def flatMapLatestStep(
        fsa: M[Step[M, A]],
        fsb: M[Step[M, B]]
    ): M[Step[M, B]] = {
      M.map(M.choose(fsa, fsb)) {
        case -\/((sa, fsb)) =>
          sa match {
            case Yield(a, s) =>
              Skip(StreamT(flatMapLatestStep(s.step, f(a).step)))
            case Skip(s) =>
              Skip(StreamT(flatMapLatestStep(s.step, fsb)))
            case Done() =>
              Skip(StreamT(fsb))
          }
        case \/-((fsa, sb)) =>
          sb match {
            case Yield(b, s) =>
              Yield(b, StreamT(flatMapLatestStep(fsa, s.step)))
            case Skip(s) =>
              Skip(StreamT(flatMapLatestStep(fsa, s.step)))
            case Done() =>
              Skip(StreamT(flatMapLatestInitStep(fsa)))
          }
      }
    }
    StreamT(flatMapLatestInitStep(this.step))
  }
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class StreamTInstances0 {
  implicit def StreamTInstance1[F[_]](implicit
      F0: Functor[F]
  ): Bind[StreamT[F, *]] with Plus[StreamT[F, *]] =
    new StreamTInstance1[F] {
      implicit def F: Functor[F] = F0
    }

  implicit def StreamTSemigroup[F[_], A](implicit
      F0: Functor[F]
  ): Semigroup[StreamT[F, A]] =
    new StreamTSemigroup[F, A] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class StreamTInstances extends StreamTInstances0 {
  implicit def StreamTMonoid[F[_], A](implicit
      F0: Applicative[F]
  ): Monoid[StreamT[F, A]] =
    new StreamTMonoid[F, A] {
      implicit def F: Applicative[F] = F0
    }
  implicit def StreamTMonadPlus[F[_]](implicit
      F0: Applicative[F]
  ): MonadPlus[StreamT[F, *]] =
    new StreamTMonadPlus[F] {
      implicit def F: Applicative[F] = F0
    }
  implicit def StreamTEqual[F[_], A](implicit
      E: Equal[F[LazyList[A]]],
      F: Monad[F]
  ): Equal[StreamT[F, A]] = E.contramap((_: StreamT[F, A]).toLazyList)
  implicit def StreamTShow[F[_], A](implicit
      E: Show[F[LazyList[A]]],
      F: Monad[F]
  ): Show[StreamT[F, A]] =
    Contravariant[Show].contramap(E)((_: StreamT[F, A]).toLazyList)
  implicit val StreamTHoist: Hoist[StreamT] = new StreamTHoist {}
  implicit def StreamTFoldable[F[_]: Foldable]: Foldable[StreamT[F, *]] =
    new Foldable[StreamT[F, *]] with Foldable.FromFoldMap[StreamT[F, *]] {
      override def foldMap[A, M: Monoid](s: StreamT[F, A])(f: A => M) =
        s.foldMap(f)
    }
  implicit def StreamTMergeMonoid[F[_], A](implicit
      F0: Nondeterminism[F]
  ): Monoid[StreamT[F, A] @@ Tags.Parallel] =
    new StreamTMergeMonoid[F, A] {
      implicit def F: Nondeterminism[F] = F0
    }
  implicit def StreamTMergeMonad[F[_]](implicit
      F0: Nondeterminism[F]
  ): Monad[λ[α => StreamT[F, α] @@ Tags.Parallel]] =
    new Monad[λ[α => StreamT[F, α] @@ Tags.Parallel]] {

      def bind[A, B](fa: StreamT[F, A] @@ Tags.Parallel)(
          f: A => StreamT[F, B] @@ Tags.Parallel
      ): StreamT[F, B] @@ Tags.Parallel =
        Tags.Parallel(
          Tags.Parallel.unwrap(fa).mergeMap(Tags.Parallel.unsubst(f))
        )

      def point[A](a: => A): StreamT[F, A] @@ Tags.Parallel =
        Tags.Parallel(StreamTMonadPlus(F0).point(a))
    }
}

object StreamT extends StreamTInstances {

  implicit def toDeferrer[M[_], A](stream: => StreamT[M, A]): Deferrer[M, A] =
    new Deferrer[M, A] {
      def #::(elem: => A)(implicit M: Applicative[M]): StreamT[M, A] =
        new StreamT[M, A] {
          def step = M.pure(Yield(elem, stream))
        }

    }

  trait Deferrer[M[_], A] {
    def #::(elem: => A)(implicit M: Applicative[M]): StreamT[M, A]
  }

  def apply[M[_], A](step0: => M[Step[M, A]]): StreamT[M, A] =
    new StreamT[M, A] {
      def step = step0
    }

  def empty[M[_], A](implicit M: Applicative[M]): StreamT[M, A] =
    StreamT[M, A](M point Done())

  def liftM[M[_], A](a: M[A])(implicit M: Applicative[M]): StreamT[M, A] =
    StreamT[M, A](M.map(a)(Yield(_, empty[M, A](M))))

  def fromLazyList[M[_], A](
      mas: M[LazyList[A]]
  )(implicit M: Applicative[M]): StreamT[M, A] = {
    def loop(as: LazyList[A]): Step[M, A] = as match {
      case head #:: tail => Yield(head, apply(M.point(loop(tail))))
      case _             => Done()
    }

    apply[M, A](M.map(mas)(loop))
  }

  def unfoldM[M[_], A, B](
      start: B
  )(f: B => M[Option[(A, B)]])(implicit M: Functor[M]): StreamT[M, A] =
    StreamT[M, A](M.map(f(start)) {
      case Some((a, b)) => Yield(a, unfoldM(b)(f))
      case None         => Done()
    })

  def unfold[A, B](b: B)(f: B => Option[(A, B)]): StreamT[Id, A] =
    unfoldM[Id, A, B](b)(f)

  def fromIterable[A](s: Iterable[A]): StreamT[Id, A] = {
    def stepper(b: Iterable[A]): Option[(A, Iterable[A])] =
      if (b.isEmpty) None else Some((b.head, b.tail))
    unfold(s)(stepper)
  }

  /** Converts a [[StreamT]] that emits [[StreamT]]s into a single [[StreamT]]
    * that emits the items emitted by the most-recently-emitted of those
    * [[StreamT]]s.
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/switch.png" />
    */
  def switch[M[_]: Nondeterminism, A](
      streams: StreamT[M, StreamT[M, A]]
  ): StreamT[M, A] = {
    def switchInitStep(
        mTailStep: M[Step[M, StreamT[M, A]]]
    ): M[Step[M, A]] = {
      Nondeterminism[M].map(mTailStep) {
        case Yield(a, s) =>
          Skip(StreamT(switchStep(a.step, s.step)))
        case Skip(s) =>
          Skip(StreamT(switchInitStep(s.step)))
        case Done() =>
          Done()
      }
    }
    def switchStep(
        mHeadStep: M[Step[M, A]],
        mTailStep: M[Step[M, StreamT[M, A]]]
    ): M[Step[M, A]] = {
      Nondeterminism[M].map(
        Nondeterminism[M].choose(mTailStep, mHeadStep)
      ) {
        case -\/((tail, mHeadStep)) =>
          tail match {
            case Yield(a, s) =>
              Skip(StreamT(switchStep(a.step, s.step)))
            case Skip(s) =>
              Skip(StreamT(switchStep(mHeadStep, s.step)))
            case Done() =>
              Skip(StreamT(mHeadStep))
          }
        case \/-((mTailStep, headStream)) =>
          headStream match {
            case Yield(b, s) =>
              Yield(b, StreamT(switchStep(s.step, mTailStep)))
            case Skip(s) =>
              Skip(StreamT(switchStep(s.step, mTailStep)))
            case Done() =>
              Skip(StreamT(switchInitStep(mTailStep)))
          }
      }
    }
    StreamT(switchInitStep(streams.step))
  }

  def wrapEffect[M[_]: Functor, A](m: M[StreamT[M, A]]): StreamT[M, A] =
    StreamT(Functor[M].map(m)(Skip(_)))

  def runStreamT[S, A](stream: StreamT[State[S, *], A], s0: S): StreamT[Id, A] =
    runStreamT[S, Id, A](stream, s0)

  def runStreamT[S, M[_], A](stream: StreamT[StateT[S, M, *], A], s0: S)(
      implicit M: Bind[M]
  ): StreamT[M, A] =
    StreamT(
      M.map(stream.step(s0)) { case (a, s) =>
        s match {
          case Yield(a1, s1) => Yield(a1, runStreamT(s1, a))
          case Skip(s1)      => Skip(runStreamT(s1, a))
          case Done()        => Done()
        }
      }
    )

  private def mergeStep[M[_], A](s1: Step[M, A], fs2: M[Step[M, A]])(implicit
      M: Nondeterminism[M]
  ): Step[M, A] = {
    s1 match {
      case Yield(a, s) =>
        Yield(a, StreamT(mergeFStep(s.step, fs2)))
      case Skip(s) =>
        Skip(StreamT(mergeFStep(s.step, fs2)))
      case Done() =>
        Skip(StreamT(fs2))
    }
  }

  private def mergeFStep[M[_], A](fs1: M[Step[M, A]], fs2: M[Step[M, A]])(
      implicit M: Nondeterminism[M]
  ): M[Step[M, A]] = {
    M.map(M.choose(fs1, fs2)) {
      case -\/((s1, fs2)) =>
        mergeStep(s1, fs2)
      case \/-((fs1, s2)) =>
        mergeStep(s2, fs1)
    }
  }

  sealed abstract class Step[M[_], A] extends Product with Serializable

  final case class Yield[M[_], A](a: A, s: StreamT[M, A]) extends Step[M, A]
  final case class Skip[M[_], A](s: StreamT[M, A]) extends Step[M, A]
  final case class Done[M[_], A] private () extends Step[M, A]
  object Done {
    def apply[M[_], A](): Step[M, A] = done_.asInstanceOf[Done[M, A]]
    // https://github.com/scala/bug/issues/11953
    private[this] final val done_ : Done[Nothing, Nothing] =
      new Done[Nothing, Nothing]()
  }
}

//
// Implementation traits for type class instances
//

private trait StreamTInstance1[F[_]]
    extends Bind[StreamT[F, *]]
    with Plus[StreamT[F, *]] {
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

private trait StreamTMonoid[F[_], A]
    extends Monoid[StreamT[F, A]]
    with StreamTSemigroup[F, A] {
  implicit def F: Applicative[F]

  def zero: StreamT[F, A] = StreamT.empty[F, A]
}

private trait StreamTMonadPlus[F[_]]
    extends MonadPlus[StreamT[F, *]]
    with StreamTInstance1[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): StreamT[F, A] = a :: StreamT.empty[F, A]

  def empty[A]: StreamT[F, A] = StreamT.empty
}

private trait StreamTHoist extends Hoist[StreamT] {
  import StreamT._

  implicit def apply[G[_]: Monad]: Monad[StreamT[G, *]] = StreamTMonadPlus[G]

  def liftM[M[_], A](a: M[A])(implicit M: Monad[M]): StreamT[M, A] =
    StreamT.liftM(a)

  override def wrapEffect[G[_]: Monad, A](a: G[StreamT[G, A]]): StreamT[G, A] =
    StreamT.wrapEffect(a)

  def hoist[M[_], N[_]](
      f: M ~> N
  )(implicit M: Monad[M]): StreamT[M, *] ~> StreamT[N, *] =
    new (StreamT[M, *] ~> StreamT[N, *]) {
      def apply[A](a: StreamT[M, A]) = a.trans(f)
    }
}

private trait StreamTMergeMonoid[F[_], A]
    extends Monoid[StreamT[F, A] @@ Tags.Parallel] {
  implicit def F: Nondeterminism[F]

  def append(
      f1: StreamT[F, A] @@ Tags.Parallel,
      f2: => StreamT[F, A] @@ Tags.Parallel
  ): StreamT[F, A] @@ Tags.Parallel =
    Tags.Parallel(Tags.Parallel.unwrap(f1).mergeWith(Tags.Parallel.unwrap(f2)))

  def zero: StreamT[F, A] @@ Tags.Parallel = Tags.Parallel(StreamT.empty)
}
