package scalaz

////
/**
 * A context supporting nondeterministic choice. Unlike `Monad.bind`,
 * which imposes a total order on the sequencing of effects throughout a
 * computation, the `choose` and `chooseAny` operations let us
 * partially order the sequencing of effects. Canonical instances are
 * `concurrent.Future` and `concurrent.Task`, which run their arguments
 * in parallel, returning whichever comes back 'first'.
 *
 * TODO - laws
 */
////
trait Nondeterminism[F[_]] extends Monad[F] { self =>
  ////

  import scalaz.Tags.Parallel
  import scalaz.std.anyVal._

  /**
   * A commutative operation which chooses nondeterministically to obtain
   * a value from either `a` or `b`. If `a` 'wins', a 'residual' context
   * for `b` is returned; if `b` wins, a residual context for `a` is
   * returned. The residual is useful for various instances like `Future`,
   * which may race the two computations and require a residual to ensure
   * the result of the 'losing' computation is not discarded.
   *
   * This function can be defined in terms of `chooseAny` or vice versa.
   * The default implementation calls `chooseAny` with a
   * two-element list and uses the `Functor` for `F` to fix up types.
   */
  def choose[A,B](a: F[A], b: F[B]): F[(  A,  F[B]) \/
                                       (F[A],   B )] =
    map(chooseAny(List[F[A \/ B]](map(a)(\/.left), map(b)(\/.right))).get) {
      (x: (A \/ B, Seq[F[A \/ B]])) => x match {
        case (-\/(a), Seq(br)) =>
          -\/((a, map(br) {
            case \/-(b) => b
            case _ => sys.error("broken residual handling in a Nondeterminism instance")
          }))
        case (\/-(b), Seq(ar)) =>
          \/-((map(ar) {
            case -\/(a) => a
            case _ => sys.error("broken residual handling in a Nondeterminism instance")
          }, b))
        case _ => sys.error("broken Nondeterminism instance tossed out a residual")
      }
    }

  /**
   * A commutative operation which chooses nondeterministically to obtain
   * a value from any of the elements of `as`. In the language of posets, this
   * constructs an antichain (a set of elements which are all incomparable) in
   * the effect poset for this computation.
   *
   * @return `None`, if the input is empty.
   */
  def chooseAny[A](a: Seq[F[A]]): Option[F[(A, Seq[F[A]])]] =
    if (a.isEmpty) None
    else Some(chooseAny(a.head, a.tail))

  def chooseAny[A](head: F[A], tail: Seq[F[A]]): F[(A, Seq[F[A]])]

  // derived functions

  /**
   * Apply a function to the results of `a` and `b`, nondeterminstically
   * ordering their effects.
   */
  def mapBoth[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    bind(choose(a, b)) {
      case -\/((a,rb)) => map(rb)(b => f(a,b))
      case \/-((ra,b)) => map(ra)(a => f(a,b))
    }


  /**
   * Apply a function to 2 results, nondeterminstically ordering their effects, alias of mapBoth
   */
  def nmap2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    mapBoth(a,b)(f)

  /**
   * Apply a function to 3 results, nondeterminstically ordering their effects
   */
  def nmap3[A,B,C,R](a: F[A], b: F[B], c: F[C])(f: (A,B,C) => R): F[R] =
    nmap2(nmap2(a, b)((_,_)), c)((ab,c) => f(ab._1, ab._2, c))

  /**
   * Apply a function to 4 results, nondeterminstically ordering their effects
   */
  def nmap4[A,B,C,D,R](a: F[A], b: F[B], c: F[C], d: F[D])(f: (A,B,C,D) => R): F[R] =
    nmap2(nmap2(a, b)((_,_)), nmap2(c,d)((_,_)))((ab,cd) => f(ab._1, ab._2, cd._1, cd._2))

  /**
   * Apply a function to 5 results, nondeterminstically ordering their effects
   */
  def nmap5[A,B,C,D,E,R](a: F[A], b: F[B], c: F[C], d: F[D], e: F[E])(f: (A,B,C,D,E) => R): F[R] =
    nmap2(nmap2(a, b)((_,_)), nmap3(c,d,e)((_,_,_)))((ab,cde) => f(ab._1, ab._2, cde._1, cde._2, cde._3))

  /**
   * Apply a function to 6 results, nondeterminstically ordering their effects
   */
  def nmap6[A,B,C,D,E,FF,R](a: F[A], b: F[B], c: F[C], d: F[D], e: F[E], ff:F[FF])(f: (A,B,C,D,E,FF) => R): F[R] =
    nmap2(nmap3(a, b, c)((_,_,_)), nmap3(d,e,ff)((_,_,_)))((abc,deff) => f(abc._1, abc._2, abc._3, deff._1, deff._2, deff._3))


  /**
   * Obtain results from both `a` and `b`, nondeterministically ordering
   * their effects.
   */
  def both[A,B](a: F[A], b: F[B]): F[(A,B)] = mapBoth(a,b)((_,_))

  /**
   * Nondeterministically gather results from the given sequence of actions
   * to a list. Same as calling `reduceUnordered` with the `List` `Monoid`.
   *
   * To preserve the order of the output list while allowing nondetermininstic
   * ordering of effects, use `gather`.
   */
  def gatherUnordered[A](fs: Seq[F[A]]): F[List[A]] =
    reduceUnordered[A, List[A]](fs)

  def gatherUnordered1[A](fs: NonEmptyList[F[A]]): F[NonEmptyList[A]] = {
    val R = implicitly[Reducer[A, List[A]]]
    bind(chooseAny(fs.head, fs.tail.toList)) { case (a, residuals) =>
      map(reduceUnordered(residuals)(R))(list => NonEmptyList.nels(a, list: _*))
    }
  }

  /**
   * Nondeterministically gather results from the given sequence of actions.
   * The result will be arbitrarily reordered, depending on the order
   * results come back in a sequence of calls to `chooseAny`.
   */
  def reduceUnordered[A, M](fs: Seq[F[A]])(implicit R: Reducer[A, M]): F[M] =
    if (fs.isEmpty) point(R.zero)
    else bind(chooseAny(fs.head, fs.tail)) { case (a, residuals) =>
      map(reduceUnordered(residuals))(R.cons(a, _))
    }

  /**
   * Nondeterministically gather results from the given sequence of actions.
   * This function is the nondeterministic analogue of `sequence` and should
   * behave identically to `sequence` so long as there is no interaction between
   * the effects being gathered. However, unlike `sequence`, which decides on
   * a total order of effects, the effects in a `gather` are unordered with
   * respect to each other.
   *
   * Although the effects are unordered, we ensure the order of results
   * matches the order of the input sequence. Also see `gatherUnordered`.
   */
  def gather[A](fs: Seq[F[A]]): F[List[A]] =
    map(gatherUnordered(fs.zipWithIndex.map { case (f,i) => strengthR(f,i) }))(
      ais => ais.sortBy(_._2).map(_._1))

  def gather1[A](fs: NonEmptyList[F[A]]): F[NonEmptyList[A]] =
    map(gatherUnordered1(fs.zipWithIndex.map { case (f,i) => strengthR(f,i) }))(
      ais => ais.sortBy(_._2).map(_._1))

  /**
   * Nondeterministically sequence `fs`, collecting the results using a `Monoid`.
   */
  def aggregate[A: Monoid](fs: Seq[F[A]]): F[A] =
    map(gather(fs))(_.foldLeft(implicitly[Monoid[A]].zero)((a,b) => implicitly[Monoid[A]].append(a,b)))

  def aggregate1[A: Semigroup](fs: NonEmptyList[F[A]]): F[A] =
    map(gather1(fs))(Foldable1[NonEmptyList].suml1(_))

  /**
   * Nondeterministically sequence `fs`, collecting the results using
   * a commutative `Monoid`.
   */
  def aggregateCommutative[A: Monoid](fs: Seq[F[A]]): F[A] =
    map(gatherUnordered(fs))(_.foldLeft(implicitly[Monoid[A]].zero)((a,b) => implicitly[Monoid[A]].append(a,b)))

  def aggregateCommutative1[A: Semigroup](fs: NonEmptyList[F[A]]): F[A] =
    map(gatherUnordered1(fs))(Foldable1[NonEmptyList].suml1(_))

  def parallel: Applicative[λ[α => F[α] @@ Parallel]] =
    new Applicative[λ[α => F[α] @@ Parallel]] {
      def point[A](a: => A) = Parallel(self.point(a))
      override def map[A, B](fa: F[A] @@ Parallel)(f: A => B) =
        Parallel(self.map(Tag.unwrap(fa))(f))
      def ap[A, B](fa: => F[A] @@ Parallel)(fab: => F[A => B] @@ Parallel) =
        Parallel(self.mapBoth(Tag.unwrap(fa), Tag.unwrap(fab))((a, f) => f(a)))
    }

  ////
  val nondeterminismSyntax = new scalaz.syntax.NondeterminismSyntax[F] { def F = Nondeterminism.this }
}

object Nondeterminism {
  @inline def apply[F[_]](implicit F: Nondeterminism[F]): Nondeterminism[F] = F

  ////

  ////
}
