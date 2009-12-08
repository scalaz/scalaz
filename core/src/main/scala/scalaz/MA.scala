package scalaz

sealed trait MA[M[_], A] {
  val v: M[A]

  import Scalaz._

  def ∘[B](f: A => B)(implicit t: Functor[M]): M[B] = t.fmap(v, f)

  def map[B](f: A => B)(implicit t: Functor[M]): M[B] = ∘(f)

  def >|[B](f: => B)(implicit t: Functor[M]): M[B] = ∘(_ => f)

  def ⊛[B](f: M[A => B])(implicit a: Apply[M]): M[B] = a(f, v)

  def <⊛>[B, C](b: M[B], z: (A, B) => C)(implicit t: Functor[M], a: Apply[M]): M[C] = a(t.fmap(v, z.curry), b)

  def <⊛⊛>[B, C, D](b: M[B], c: M[C], z: (A, B, C) => D)(implicit t: Functor[M], a: Apply[M]): M[D] = a(a(t.fmap(v, z.curry), b), c)

  def <⊛⊛⊛>[B, C, D, E](b: M[B], c: M[C], d: M[D], z: (A, B, C, D) => E)(implicit t: Functor[M], a: Apply[M]): M[E] = a(a(a(t.fmap(v, z.curry), b), c), d)

  def <⊛⊛⊛⊛>[B, C, D, E, F](b: M[B], c: M[C], d: M[D], e: M[E], z: (A, B, C, D, E) => F)(implicit t: Functor[M], a: Apply[M]): M[F] = a(a(a(a(t.fmap(v, z.curry), b), c), d), e)

  def ⊛>[B](b: M[B])(implicit t: Functor[M], a: Apply[M]): M[B] = <⊛>(b, (_, b: B) => b)

  def <⊛[B](b: M[B])(implicit t: Functor[M], a: Apply[M]): M[A] = <⊛>(b, (a, _: B) => a)

  def <×>[B](b: M[B])(implicit t: Functor[M], a: Apply[M]): M[(A, B)] = <⊛>(b, (_: A, _: B))

  def <××>[B, C](b: M[B], c: M[C])(implicit t: Functor[M], a: Apply[M]): M[(A, B, C)] = <⊛⊛>(b, c, (_: A, _: B, _: C))

  def <×××>[B, C, D](b: M[B], c: M[C], d: M[D])(implicit t: Functor[M], a: Apply[M]): M[(A, B, C, D)] = <⊛⊛⊛>(b, c, d, (_: A, _: B, _: C, _: D))

  def <××××>[B, C, D, E](b: M[B], c: M[C], d: M[D], e: M[E])(implicit t: Functor[M], a: Apply[M]): M[(A, B, C, D, E)] = <⊛⊛⊛⊛>(b, c, d, e, (_: A, _: B, _: C, _: D, _: E))

  def ↦[F[_], B](f: A => F[B])(implicit a: Applicative[F], t: Traverse[M]): F[M[B]] =
    t.traverse(f, v)

  def ∗[B](f: A => M[B])(implicit b: Bind[M]): M[B] = b.bind(v, f)

  def >>=[B](f: A => M[B])(implicit b: Bind[M]): M[B] = ∗(f)

  def ∗|[B](f: => M[B])(implicit b: Bind[M]): M[B] = ∗(_ => f)

  def >>=|[B](f: => M[B])(implicit b: Bind[M]): M[B] = ∗|(f)

  def flatMap[B](f: A => M[B])(implicit b: Bind[M]): M[B] = ∗(f)

  def join[B](implicit m: A <:< M[B], b: Bind[M]): M[B] = ∗(z => z)

  def μ[B](implicit m: A <:< M[B], b: Bind[M]): M[B] = join

  def ∞[B](implicit b: Bind[M]): M[B] = v ∗| v.∞

  def ⟴(z: => M[A])(implicit p: Plus[M]): M[A] = p.plus(v, z)

  def ➜:(a: A)(implicit s: Semigroup[M[A]], q: Pure[M]): M[A] = s append (q.pure(a), v)

  def ➝:(a: A)(implicit p: Plus[M], q: Pure[M]): M[A] = p.plus(q.pure(a), v)

  def ➡(f: A => Unit)(implicit e: Each[M]): Unit = e.each(v, f)

  def foreach(f: A => Unit)(implicit e: Each[M]): Unit = ➡ (f)

  def foldl[B](b: B, f: (B, A) => B)(implicit r: FoldLeft[M]): B = r.foldLeft[B, A](v, b, f)

  def foldl1(f: (A, A) => A)(implicit r: FoldLeft[M]): Option[A] = foldl[Option[A]](None, (a1, a2) => Some(a1 match {
    case None => a2
    case Some(x) => f(a2, x)
  }))

  def listl(implicit r: FoldLeft[M]): List[A] = {
    val b = new scala.collection.mutable.ListBuffer[A]
    foldl[scala.Unit]((), (_, a) => b += a)
    b.toList
  }

  def ∑(implicit r: FoldLeft[M], m: Monoid[A]): A = foldl[A](m.zero, m append (_, _))

  def ♯(implicit r: FoldLeft[M]): Int = foldl[Int](0, (b, _) => b + 1)

  def len(implicit l: Length[M]): Int = l len v

  def max(implicit r: FoldLeft[M], ord: Order[A]): Option[A] =
    foldl1((x: A, y: A) => if (x ≩ y) x else y)

  def min(implicit r: FoldLeft[M], ord: Order[A]): Option[A] =
    foldl1((x: A, y: A) => if (x ≨ y) x else y)

  def longDigits(implicit d: A <:< Digit, t: FoldLeft[M]): Long =
    foldl[Long](0L, (n, a) => n * 10L + (a: Digit))

  def digits(implicit c: A <:< Char, t: Functor[M]): M[Option[Digit]] =
    ∘((a: A) => (a: Char).digit)

  def sequence[N[_], B](implicit a: A <:< N[B], t: Traverse[M], n: Applicative[N]): N[M[B]] =
    ↦((z: A) => (z: N[B]))

  def traverseDigits(implicit c: A <:< Char, t: Traverse[M]): Option[M[Digit]] = {
    val k = ∘((f: A) => (f: Char)).digits.sequence
    k
  }

  def foldr[B](b: B, f: (A, => B) => B)(implicit r: FoldRight[M]): B = r.foldRight(v, b, f)

  def foldr1(f: (A, => A) => A)(implicit r: FoldRight[M]): Option[A] = foldr[Option[A]](None, (a1, a2) => Some(a2 match {
    case None => a1
    case Some(x) => f(a1, x)
  }))

  def ∑∑(implicit r: FoldRight[M], m: Monoid[A]): A = foldr[A](m.zero, m append (_, _))

  def foldMap[B](f: A => B)(implicit r: FoldRight[M], m: Monoid[B]): B = foldr[B](m.zero, (a, b) => m.append(f(a), b))

  def listr(implicit r: FoldRight[M]): List[A] = foldr[List[A]](Nil, _ :: _)

  def stream(implicit r: FoldRight[M]): Stream[A] = foldr[Stream[A]](Stream.empty, Stream.cons(_, _))

  def !!(n: Int)(implicit r: FoldRight[M]): A = stream(r)(n)

  def !(n: Int)(implicit i: Index[M]): Option[A] = i.index(v, n)

  def -!-(n: Int)(implicit i: Index[M]): A = this.!(n) getOrElse (error("Index " + n + " out of bounds"))

  def ∃(p: A => Boolean)(implicit r: FoldRight[M]): Boolean = foldr[Boolean](false, p(_) || _)

  def ∀(p: A => Boolean)(implicit r: FoldRight[M]): Boolean = foldr[Boolean](true, p(_) && _)

  def empty(implicit r: FoldRight[M]): Boolean = ∀(_ => false)

  def splitWith(p: A => Boolean)(implicit r: FoldRight[M]): List[List[A]] =
    foldr[(List[List[A]], Option[Boolean])]((Nil, None), (a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => List(List(a))
        case (x, Some(q)) => if (pa == q) (a :: x.head) :: x.tail else List(a) :: x
      }, Some(pa))
    })._1

  def selectSplit(p: A => Boolean)(implicit r: FoldRight[M]): List[List[A]] =
    foldr[(List[List[A]], Boolean)]((Nil, false), (a, xb) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if (pa)
          if (b)
            (a :: x.head) :: x.tail else
            List(a) :: x
        else x, pa)
      }
    })._1

  def para[B](b: B, f: (=> A, => M[A], B) => B)(implicit p: Paramorphism[M]): B = p.para(v, b, f)

  def ↣[B](f: A => B)(implicit t: Traverse[M], m: Monoid[B]): B = {
    case class Acc[B, A](acc: B)

    implicit val AccApply = new Apply[PartialApply1Of2[Acc, B]#Apply] {
      def apply[A, X](f: Acc[B, A => X], fa: Acc[B, A]) = Acc[B, X](m append (f.acc, fa.acc))
    }

    implicit val AccPure = new Pure[PartialApply1Of2[Acc, B]#Apply] {
      def pure[A](a: => A) = Acc[B, A](m.zero)
    }

    implicit val AccApplicative = Applicative.applicative[PartialApply1Of2[Acc, B]#Apply](AccPure, AccApply)

    t.traverse[PartialApply1Of2[Acc, B]#Apply, A, B](a => Acc[B, B](f(a)), v).acc
  }

  def collapse(implicit t: Traverse[M], m: Monoid[A]): A = ↣(identity[A])

  def =>>[B](f: M[A] => B)(implicit w: Comonad[M]): M[B] = w.fmap(w.cojoin(v), f)

  def copure[B](implicit p: Copure[M]): A = p copure v

  def ε[B](implicit p: Copure[M]): A = copure

  def cojoin(implicit j: Cojoin[M]): M[M[A]] = j cojoin v

  def υ(implicit j: Cojoin[M]): M[M[A]] = cojoin

  def <--->(w: M[A])(implicit l: Length[M], ind: Index[M], equ: Equal[A]): Int = {
    def levenshteinMatrix(w: M[A])(implicit l: Length[M], ind: Index[M], equ: Equal[A]): (Int, Int) => Int = {
      val m = mutableHashMapMemo[(Int, Int), Int]

      def get(i: Int, j: Int): Int = if (i == 0) j else if (j == 0) i else {
        lazy val t = this -!- (i - 1)
        lazy val u = w -!- (j - 1)
        lazy val e = t ≟ u

        val g = m {case (a, b) => get(a, b)}
        val a = g(i - 1, j) + 1
        val b = g(i - 1, j - 1) + (if (e) 0 else 1)
        def c = g(i, j - 1) + 1
        if (a < b) a else if (b <= c) b else c
      }

      get
    }

    val k = levenshteinMatrix(w)
    k(l.len(v), l.len(w))
  }

  def foldLeftM[N[_], B](f: (B, A) => N[B], b: B)(implicit fr: FoldLeft[M], m: Monad[N]): N[B] =
    foldl[N[B]](b η, (b, a) => b ∗ ((z: B) => f(z, a)))

  def foldRightM[N[_], B](f: (B, A) => N[B], b: B)(implicit fr: FoldRight[M], m: Monad[N]): N[B] =
    foldr[N[B]](b η, (a, b) => b ∗ ((z: B) => f(z, a)))

  def replicateM[N[_]](n: Int)(implicit m: Monad[M], p: Pure[N], d: Monoid[N[A]]): M[N[A]] =
    if (n <= 0) ∅ η
    else v ∗ (a => replicateM[N](n - 1) ∘ (a ➜: _) )

  def zipWithA[F[_], B, C](b: M[B], f: (A, B) => F[C])(implicit a: Applicative[M], t: Traverse[M], z: Applicative[F]): F[M[C]] =
    (b ⊛ (a.fmap(v, f.curry))).sequence[F, C]

  def bktree(implicit f: FoldLeft[M], m: MetricSpace[A]) =
    foldl[BKTree[A]](emptyBKTree, _ + _)

  import concurrent.Strategy

  def parMap[B](f: A => B)(implicit m: Functor[M], s: Strategy[B]): () => M[B] =
    ∘(f.concurry).parM

  def parBind[B](f: A => M[B])(implicit m: Bind[M], t: Functor[M], s: Strategy[M[B]]): () => M[B] =
    parMap(f).map(((_: MA[M, M[B]]) μ) compose (ma(_)))

  def parZipWith[B, C](f: (A, B) => C, bs: M[B])(implicit z: Applicative[M], s: Strategy[C]): () => M[C] =
    (bs ⊛ (v ∘ f.concurry.curry)).parM

  import scalaz.concurrent.Strategy

  def parM[B](implicit b: A <:< (() => B), m: Functor[M], s: Strategy[B]): () => M[B] =
    () => v ∘ (z => s(z).apply)
}

// Previously there was an ambiguity because (A => B) could be considered as MA[(R => _), A] or MA[(_ => R), A].
// This is a hack to fix the pressing problem that this caused.
trait MACofunctor[M[_], A] {
  val v: M[A]

  def ∙[B](f: B => A)(implicit t: Cofunctor[M]) = t.comap(v, f)

  def |<[B](f: => A)(implicit t: Cofunctor[M]) = ∙((_: B) => f)
}


trait MAsLow {
  implicit def maImplicit[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val v = a
  }

  implicit def maCofunctorImplicit[M[_], A](a: M[A]): MACofunctor[M, A] = new MACofunctor[M, A] {
    val v = a
  }
}

trait MAs {
  def ma[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val v = a
  }

  def maCofunctor[M[_], A](a: M[A])(implicit cf: Cofunctor[M]): MACofunctor[M, A] = new MACofunctor[M, A] {
    val v = a
  }
}
