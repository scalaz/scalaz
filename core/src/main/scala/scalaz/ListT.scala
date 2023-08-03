package scalaz

import Id._

/** ListT monad transformer.
  */
sealed abstract class ListT[M[_], A]
    extends Product
    with Serializable {

  import ListT._

  def uncons(implicit M: Monad[M]): M[Option[(A, ListT[M, A])]] =
    this match {
      case Yield(a, m) => M.point(Some((a, Skip(m))))
      case Skip(m)     => M.bind(m)(_.uncons)
      case Done()      => M.point(None)
    }

  def unconsRec(implicit
      M: BindRec[M],
      A: Applicative[M]
  ): M[Option[(A, ListT[M, A])]] =
    M.tailrecM(this) {
      case Yield(a, m) => A.pure(\/-(Some((a, Skip(m)))))
      case Skip(m)     => M.map(m)(-\/(_))
      case Done()      => A.pure(\/-(None))
    }

  def ::(a: A)(implicit M: Applicative[M]): ListT[M, A] =
    Yield(a, M.pure(this))

  def isEmpty(implicit M: Monad[M]): M[Boolean] = M.map(uncons)(_.isEmpty)
  def isEmptyRec(implicit M: BindRec[M], A: Applicative[M]): M[Boolean] =
    M.map(unconsRec)(_.isEmpty)

  def head(implicit M: Monad[M]): M[A] =
    M.map(uncons)(_.getOrElse(sys.error("head: empty ListT"))._1)
  def headRec(implicit M: BindRec[M], A: Applicative[M]): M[A] =
    M.map(unconsRec)(_.getOrElse(sys.error("head: empty ListT"))._1)

  def headOption(implicit M: Monad[M]): M[Option[A]] =
    M.map(uncons)(_.map(_._1))
  def headOptionRec(implicit M: BindRec[M], A: Applicative[M]): M[Option[A]] =
    M.map(unconsRec)(_.map(_._1))

  def tailM(implicit M: Monad[M]): M[ListT[M, A]] =
    M.map(uncons)(_.getOrElse(sys.error("tailM: empty ListT"))._2)
  def tailMRec(implicit
      M: BindRec[M],
      A: Applicative[M]
  ): M[ListT[M, A]] =
    M.map(unconsRec)(_.getOrElse(sys.error("tailM: empty ListT"))._2)

  def tailOption(implicit M: Monad[M]): M[Option[ListT[M, A]]] =
    M.map(uncons)(_.map(_._2))
  def tailOptionRec(implicit
      M: BindRec[M],
      A: Applicative[M]
  ): M[Option[ListT[M, A]]] =
    M.map(unconsRec)(_.map(_._2))

  def trans[N[_]](t: M ~> N)(implicit M: Functor[M]): ListT[N, A] =
    this match {
      case Yield(a, m) => Yield(a, t(M.map(m)(_.trans(t))))
      case Skip(m)     => Skip(t(M.map(m)(_.trans(t))))
      case Done()      => Done()
    }

  def filter(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] =
    this match {
      case Yield(a, m) =>
        if (p(a)) Yield(a, M.map(m)(_.filter(p)))
        else Skip(M.map(m)(_.filter(p)))
      case Skip(m) => Skip(M.map(m)(_.filter(p)))
      case Done()  => Done()
    }

  def collect[B](
      pf: PartialFunction[A, B]
  )(implicit M: Functor[M]): ListT[M, B] =
    this match {
      case Yield(pf(b), m) =>
        Yield(b, M.map(m)(_.collect(pf)))
      case Yield(_, m) =>
        Skip(M.map(m)(_.collect(pf)))
      case Skip(m) =>
        Skip(M.map(m)(_.collect(pf)))
      case Done() =>
        Done()
    }

  def drop(n: Int)(implicit M: Functor[M]): ListT[M, A] = this match {
    case Yield(a, m) => if (n > 0) Skip(M.map(m)(_ drop (n - 1))) else this
    case Skip(m)     => Skip(M.map(m)(_ drop n))
    case Done()      => Done()
  }

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] =
    this match {
      case Yield(a, m) => if (p(a)) Skip(M.map(m)(_ dropWhile p)) else this
      case Skip(m)     => Skip(M.map(m)(_ dropWhile p))
      case Done()      => Done()
    }

  def postScanLeft[B](
      head: B
  )(op: (B, A) => B)(implicit M: Functor[M]): ListT[M, B] =
    this match {
      case Yield(a, m) =>
        val next = op(head, a)
        Yield(next, M.map(m)(_.postScanLeft(next)(op)))
      case Skip(m) =>
        Skip(M.map(m)(_.postScanLeft(head)(op)))
      case Done() =>
        Done()
    }

  /** Produces a [[ListT]] containing cumulative results of applying
    * the operator going left to right, including the initial value.
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
  ): ListT[M, B] =
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
    *   a new [[ListT]] containing the prefix scan of the elements in
    *   this $coll
    */
  def scan[B >: A](
      op: (B, B) => B
  )(implicit M: Functor[M]): ListT[M, B] =
    this match {
      case Yield(a, m) =>
        Yield(a, M.map(m)(_.postScanLeft[B](a)(op)))
      case Skip(m) =>
        Skip(M.map(m)(_.scan(op)))
      case Done() =>
        Done()
    }

  def take(n: Int)(implicit M: Functor[M]): ListT[M, A] = this match {
    case Yield(a, m) =>
      if (n <= 0) Done() else Yield(a, M.map(m)(_ take (n - 1)))
    case Skip(m) => Skip(M.map(m)(_ take n))
    case Done()  => Done()
  }

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] =
    this match {
      case Yield(a, m) =>
        if (p(a)) Yield(a, M.map(m)(_ takeWhile p)) else Done()
      case Skip(m) => Skip(M.map(m)(_ takeWhile p))
      case Done()  => Done()
    }

  def ++(
      bs: => ListT[M, A]
  )(implicit M: Functor[M]): ListT[M, A] =
    this match {
      case Yield(a, m) => Yield(a, M.map(m)(_ ++ bs))
      case Skip(m)     => Skip(M.map(m)(_ ++ bs))
      case Done()      => bs
    }

  def flatMap[B](
      f: A => ListT[M, B]
  )(implicit M: Functor[M]): ListT[M, B] =
    this match {
      case Yield(a, m) => f(a) ++ Skip(M.map(m)(_ flatMap f))
      case Skip(m)     => Skip(M.map(m)(_ flatMap f))
      case Done()      => Done()
    }

  def map[B](f: A => B)(implicit M: Functor[M]): ListT[M, B] =
    this match {
      case Yield(a, m) => Yield(f(a), M.map(m)(_.map(f)))
      case Skip(m)     => Skip(M.map(m)(_.map(f)))
      case Done()      => Done()
    }

  def mapM[B](f: A => M[B])(implicit M: Functor[M]): ListT[M, B] =
    this match {
      case Yield(a, m) => Skip(M.map(f(a))(Yield(_, M.map(m)(_ mapM f))))
      case Skip(m)     => Skip(M.map(m)(_ mapM f))
      case Done()      => Done()
    }

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit M: Monad[M]): M[B] =
    this match {
      case Yield(a, m) => M.bind(m)(_.foldLeft(f(z, a))(f))
      case Skip(m)     => M.bind(m)(_.foldLeft(z)(f))
      case Done()      => M.point(z)
    }

  def foldLeftRec[B](
      z: B
  )(f: (B, A) => B)(implicit M: BindRec[M], A: Applicative[M]): M[B] =
    M.tailrecM((this, z)) {
      case (Yield(a, m), z) => M.map(m)(s => -\/((s, f(z, a))))
      case (Skip(m), z)     => M.map(m)(s => -\/((s, z)))
      case (Done(), z)      => A.pure(\/-(z))
    }

  /** **Warning:** Requires evaluation of the whole stream. Depending on the
    * monad `M`, the evaluation will happen either immediately, or will be
    * deferred until the resulting `List` is extracted from the returned `M`.
    */
  def toList(implicit M: Monad[M]): M[List[A]] =
    this match {
      case Yield(a, m) =>
        M.bind(m) { s => M.map(s.toList)(a :: _) }
      case Skip(m) =>
        M.bind(m)(_.toList)
      case Done() =>
        M.pure(Nil)
    }

  /** **Warning:** Requires evaluation of the whole stream. Depending on the
    * monad `M`, the evaluation will happen either immediately, or will be
    * deferred until the resulting `List` is extracted from the returned `M`.
    */
  def toListRec(implicit M: BindRec[M], A: Applicative[M]): M[List[A]] =
    M.map(foldLeftRec(List.empty[A])(_.prepended(_)))(_.reverse)

  def foldRightRec[B](
      z: B
  )(f: (A, B) => B)(implicit M: BindRec[M], A: Applicative[M]): M[B] =
    M.map(foldLeftRec(List.empty[A])(_.prepended(_))) {
      _.foldLeft(z)((a, b) => f(b, a))
    }

  def foldMap[B](f: A => B)(implicit M: Foldable[M], B: Monoid[B]): B =
    this match {
      case Yield(a, s) => B.append(f(a), M.foldMap(s)(_.foldMap(f)))
      case Skip(s)     => M.foldMap(s)(_.foldMap(f))
      case Done()      => B.zero
    }

  def length(implicit m: Monad[M]): M[Int] =
    foldLeft(0)((c, a) => 1 + c)

  def lengthRec(implicit M: BindRec[M], A: Applicative[M]): M[Int] =
    foldLeftRec(0)((c, a) => 1 + c)

  def foreach(f: A => M[Unit])(implicit M: Monad[M]): M[Unit] =
    this match {
      case Yield(a, s) => M.bind(f(a))(_ => M.bind(s)(_.foreach(f)))
      case Skip(s)     => M.bind(s)(_.foreach(f))
      case Done()      => M.pure(())
    }

  def foreachRec(
      f: A => M[Unit]
  )(implicit B: BindRec[M], A: Applicative[M]): M[Unit] =
    B.tailrecM(this) {
      case Yield(a, m) => B.bind(f(a))(_ => B.map(m)(-\/(_)))
      case Skip(m)     => B.map(m)(-\/(_))
      case Done()      => A.pure(\/-(()))
    }

  /** Returns a new [[ListT]] only containing items that are different
    * from their previous items.
    *
    * @param `latest`
    *   the assuming previous item of the head element of this
    *   [[ListT]]
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/distinctUntilChanged.png"
    * />
    */
  def distinctUntilChanged(
      latest: A
  )(implicit M: Functor[M], A: Equal[A]): ListT[M, A] =
    this match {
      case Skip(m) =>
        Skip(M.map(m)(_.distinctUntilChanged(latest)))
      case Done() =>
        Done()
      case Yield(a, m) if A.equal(a, latest) =>
        Skip(M.map(m)(_.distinctUntilChanged(latest)))
      case Yield(a, m) =>
        Yield(a, M.map(m)(_.distinctUntilChanged(latest)))
    }

  /** Returns a new [[ListT]] only containing items that are different
    * from their previous items.
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/distinctUntilChanged.png"
    * />
    */
  def distinctUntilChanged(implicit
      M: Functor[M],
      A: Equal[A]
  ): ListT[M, A] =
    this match {
      case Skip(m) =>
        Skip(M.map(m)(_.distinctUntilChanged))
      case Done() =>
        Done()
      case Yield(a, m) =>
        Yield(a, M.map(m)(_.distinctUntilChanged(a)))
    }

  def mergeWithM(
      m2: M[ListT[M, A]]
  )(implicit M: Nondeterminism[M]): ListT[M, A] =
    this match {
      case Done() =>
        Skip(m2)
      case Skip(m1) =>
        Skip(M.map(M.choose(m1, m2)) {
          case -\/((s1, m2)) =>
            s1.mergeWithM(m2)
          case \/-((m1, s2)) =>
            s2.mergeWithM(m1)
        })
      case Yield(a, m1) =>
        Yield(
          a,
          M.map(M.choose(m1, m2)) {
            case -\/((s1, m2)) =>
              s1.mergeWithM(m2)
            case \/-((m1, s2)) =>
              s2.mergeWithM(m1)
          }
        )
    }

  def mergeWith(
      that: ListT[M, A]
  )(implicit M: Nondeterminism[M]): ListT[M, A] = {
    this match {
      case Done() =>
        that
      case Skip(m) =>
        that.mergeWithM(m)
      case Yield(a, m) =>
        Yield(a, M.pure(that.mergeWithM(m)))
    }
  }

  /** The [[flatMapLatest]] operator behaves much like the [[mergeMap]] except
    * that whenever a new item is emitted by the source [[ListT]], it
    * will not subscribe to and stop mirroring the [[ListT]] that was
    * generated from the previously-emitted item, and begin only mirroring the
    * current one.
    *
    * <img style="max-width: 100%"
    * src="http://reactivex.io/documentation/operators/images/flatMapLatest.png"/>
    */
  def flatMapLatest[B](
      f: A => ListT[M, B]
  )(implicit M: Nondeterminism[M]): ListT[M, B] = {
    this match {
      case Done() =>
        Done()
      case Skip(m) =>
        Skip(M.map(m)(_.flatMapLatest(f)))
      case Yield(a, m) =>
        Skip(m).flatMapLatest(f, f(a))
    }
  }

  def flatMapLatest[B](
      f: A => ListT[M, B],
      default: ListT[M, B]
  )(implicit M: Nondeterminism[M]): ListT[M, B] = {
    this match {
      case Done() =>
        default
      case Skip(ma) =>
        default match {
          case Done() =>
            flatMapLatest(f)
          case Skip(mb) =>
            Skip(M.map(M.choose(ma, mb)) {
              case -\/((sa, mb)) =>
                sa.flatMapLatest(f, Skip(mb))
              case \/-((ma, sb)) =>
                Skip(ma).flatMapLatest(f, sb)
            })
          case Yield(b, mb) =>
            Yield(
              b,
              M.map(M.choose(ma, mb)) {
                case -\/((sa, mb)) =>
                  sa.flatMapLatest(f, Skip(mb))
                case \/-((ma, sb)) =>
                  Skip(ma).flatMapLatest(f, sb)
              }
            )
        }
      case Yield(a, ma) =>
        Skip(ma).flatMapLatest(f, f(a))
    }
  }
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class ListTInstances0 {
  implicit def ListTInstance1[F[_]](implicit
      F0: Functor[F]
  ): Bind[ListT[F, *]] with Plus[ListT[F, *]] =
    new ListTInstance1[F] {
      implicit def F: Functor[F] = F0
    }

  implicit def ListTSemigroup[F[_], A](implicit
      F0: Functor[F]
  ): Semigroup[ListT[F, A]] =
    new ListTSemigroup[F, A] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class ListTInstances extends ListTInstances0 {
  implicit def ListTMonoid[F[_], A](implicit
      F0: Applicative[F]
  ): Monoid[ListT[F, A]] =
    new ListTMonoid[F, A] {
      implicit def F: Applicative[F] = F0
    }
  implicit def ListTMonadPlus[F[_]](implicit
      F0: Applicative[F]
  ): MonadPlus[ListT[F, *]] =
    new ListTMonadPlus[F] {
      implicit def F: Applicative[F] = F0
    }
  implicit def ListTEqual[F[_], A](implicit
      E: Equal[F[List[A]]],
      F: Monad[F]
  ): Equal[ListT[F, A]] =
    E.contramap((_: ListT[F, A]).toList)
  implicit def ListTShow[F[_], A](implicit
      E: Show[F[List[A]]],
      F: Monad[F]
  ): Show[ListT[F, A]] =
    Contravariant[Show].contramap(E)((_: ListT[F, A]).toList)
  implicit val ListTHoist: Hoist[ListT] =
    new ListTHoist {}
  implicit def ListTFoldable[F[_]: Foldable]
      : Foldable[ListT[F, *]] =
    new Foldable[ListT[F, *]]
      with Foldable.FromFoldMap[ListT[F, *]] {
      override def foldMap[A, M: Monoid](s: ListT[F, A])(f: A => M) =
        s.foldMap(f)
    }
}

object ListT extends ListTInstances {

  def empty[M[_], A]: ListT[M, A] =
    Done()

  def liftM[M[_], A](a: M[A])(implicit
      M: Applicative[M]
  ): ListT[M, A] =
    Skip(M.map(a)(Yield(_, M.pure(Done()))))

  def fromList[M[_], A](
      ml: M[List[A]]
  )(implicit M: Applicative[M]): ListT[M, A] = {
    def loop(l: List[A]): ListT[M, A] =
      l match {
        case Nil =>
          Done()
        case head :: tail =>
          Yield(head, M.pure(loop(tail)))
      }
    Skip(M.map(ml)(loop))
  }

  def unfoldM[M[_], A, B](
      start: B
  )(
      f: B => M[Option[(A, B)]]
  )(implicit M: Functor[M]): ListT[M, A] = {
    def loop(b: B): M[ListT[M, A]] = M.map(f(b))({
      case Some((a, b)) => Yield(a, loop(b))
      case None         => Done()
    })
    Skip(loop(start))
  }

  def unfold[A, B](b: B)(f: B => Option[(A, B)]): ListT[Id, A] =
    unfoldM[Id, A, B](b)(f)

  def fromIterable[M[_], A](
      s: Iterable[A]
  )(implicit M: Applicative[M]): ListT[M, A] = {
    unfoldM(s) { b =>
      M.pure(if (b.isEmpty) None else Some((b.head, b.tail)))
    }
  }

  def runListT[S, A](
      stream: ListT[State[S, *], A],
      s0: S
  ): ListT[Id, A] =
    runListT[S, Id, A](stream, s0)

  def runListT[S, M[_], A](stream: ListT[StateT[S, M, *], A], s0: S)(
      implicit M: Bind[M]
  ): ListT[M, A] =
    stream match {
      case Yield(a, m) =>
        Yield(
          a,
          M.map(m(s0)) { case (a1, s1) =>
            runListT(s1, a1)
          }
        )
      case Skip(m) =>
        Skip(
          M.map(m(s0)) { case (a1, s1) =>
            runListT(s1, a1)
          }
        )
      case Done() => Done()
    }

  final case class Yield[M[_], A](a: A, s: M[ListT[M, A]])
      extends ListT[M, A]
  final case class Skip[M[_], A](s: M[ListT[M, A]])
      extends ListT[M, A]
  final case class Done[M[_], A] private () extends ListT[M, A]
  object Done {
    def apply[M[_], A](): ListT[M, A] = done_.asInstanceOf[Done[M, A]]
    // https://github.com/scala/bug/issues/11953
    private[this] final val done_ : Done[Nothing, Nothing] =
      new Done[Nothing, Nothing]()
  }
}

//
// Implementation traits for type class instances
//

private trait ListTInstance1[F[_]]
    extends Bind[ListT[F, *]]
    with Plus[ListT[F, *]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: ListT[F, A])(f: A => B) =
    fa map f

  override final def bind[A, B](fa: ListT[F, A])(
      f: A => ListT[F, B]
  ) =
    fa flatMap f

  override final def plus[A](
      a: ListT[F, A],
      b: => ListT[F, A]
  ) =
    a ++ b
}

private trait ListTSemigroup[F[_], A]
    extends Semigroup[ListT[F, A]] {
  implicit def F: Functor[F]

  def append(
      f1: ListT[F, A],
      f2: => ListT[F, A]
  ): ListT[F, A] = f1 ++ f2
}

private trait ListTMonoid[F[_], A]
    extends Monoid[ListT[F, A]]
    with ListTSemigroup[F, A] {
  implicit def F: Applicative[F]

  def zero: ListT[F, A] = ListT.empty[F, A]
}

private trait ListTMonadPlus[F[_]]
    extends MonadPlus[ListT[F, *]]
    with ListTInstance1[F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): ListT[F, A] = a :: ListT.empty[F, A]

  def empty[A]: ListT[F, A] = ListT.empty
}

private trait ListTHoist extends Hoist[ListT] {
  import ListT._

  implicit def apply[G[_]: Monad]: Monad[ListT[G, *]] =
    ListTMonadPlus[G]

  def liftM[M[_], A](a: M[A])(implicit M: Monad[M]): ListT[M, A] =
    ListT.liftM(a)

  def hoist[M[_], N[_]](
      f: M ~> N
  )(implicit M: Monad[M]): ListT[M, *] ~> ListT[N, *] =
    new (ListT[M, *] ~> ListT[N, *]) {
      def apply[A](a: ListT[M, A]) = a.trans(f)
    }
}

private trait ListTMergeMonoid[F[_], A]
    extends Monoid[ListT[F, A] @@ Tags.Parallel] {
  implicit def F: Nondeterminism[F]

  def append(
      f1: ListT[F, A] @@ Tags.Parallel,
      f2: => ListT[F, A] @@ Tags.Parallel
  ): ListT[F, A] @@ Tags.Parallel =
    Tags.Parallel(Tags.Parallel.unwrap(f1).mergeWith(Tags.Parallel.unwrap(f2)))

  def zero: ListT[F, A] @@ Tags.Parallel =
    Tags.Parallel(ListT.empty)
}
