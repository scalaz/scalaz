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
    map(chooseAny(List[F[A \/ B]](map(a)(-\/(_)), map(b)(\/-(_)))).get) {
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
   * Obtain results from both `a` and `b`, nondeterministically ordering
   * their effects.
   */
  def both[A,B](a: F[A], b: F[B]): F[(A,B)] = mapBoth(a,b)((_,_))

  /**
   * Nondeterministically gather results from the given sequence of actions.
   * The resulting list will be arbitrarily reordered, depending on the order
   * results come back in a sequence of calls to `chooseAny`.
   *
   * To preserve the order of the output list while allowing nondetermininstic
   * ordering of effects, use `gather`.
   */
  def gatherUnordered[A](fs: Seq[F[A]]): F[List[A]] =
    if (fs.isEmpty) point(List())
    else bind(chooseAny(fs.head, fs.tail)) { case (a, residuals) =>
      map(gatherUnordered(residuals))(a :: _)
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

  /**
   * Nondeterministically sequence `fs`, collecting the results using a `Monoid`.
   */
  def aggregate[A,M](fs: Seq[F[A]])(m: Monoid[A]): F[A] =
    map(gather(fs))(_.foldLeft(m.zero)((a,b) => m.append(a,b)))

  /**
   * Nondeterministically sequence `fs`, collecting the results using
   * a commutative `Monoid`.
   */
  def aggregateCommutative[A,M](fs: Seq[F[A]])(m: Monoid[A]): F[A] =
    map(gatherUnordered(fs))(_.foldLeft(m.zero)((a,b) => m.append(a,b)))

  ////
  val nondeterminismSyntax = new scalaz.syntax.NondeterminismSyntax[F] { def F = Nondeterminism.this }
}

object Nondeterminism {
  @inline def apply[F[_]](implicit F: Nondeterminism[F]): Nondeterminism[F] = F

  ////

  ////
}
