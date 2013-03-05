package scalaz

////
/**
 * A context supporting nondeterministic choice. Unlike `Monad`, which
 * imposes a total order on the sequencing of effects throughout a
 * computation, the `choose` and `chooseAny` operations let us
 * partially order the sequencing of effects. Canonical instances are
 * `concurrent.Future` and `concurrent.Task`, which run their arguments
 * in parallel, returning whichever comes back 'first'. 
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
  def choose[A,B](a: F[A], b: F[B]): F[(  A,  F[B]) Either
                                       (F[A],   B )] = 
    map(chooseAny(List[F[Either[A,B]]](map(a)(Left(_)), map(b)(Right(_))))) { 
      (x: (Either[A,B], Seq[F[Either[A,B]]])) => x match {
        case (Left(a), Seq(br)) => 
          Left((a, map(br) { 
            case Right(b) => b
            case _ => sys.error("broken residual handling in a Nondeterminism instance") 
          }))
        case (Right(b), Seq(ar)) =>
          Right((map(ar) { 
            case Left(a) => a
            case _ => sys.error("broken residual handling in a Nondeterminism instance") 
          }, b))
        case _ => sys.error("broken Nondeterminism instance tossed out a residual")
      }
    }

  /** 
   * A commutative operation which chooses nondeterministically to obtain 
   * a value from any of the elements of `as`. Can be defined in terms of
   * `choose` or vice versa.
   */
  def chooseAny[A](a: Seq[F[A]]): F[(A, Seq[F[A]])] 

  // derived functions

  ////
  val nondeterminismSyntax = new scalaz.syntax.NondeterminismSyntax[F] { def F = Nondeterminism.this }
}

object Nondeterminism {
  @inline def apply[F[_]](implicit F: Nondeterminism[F]): Nondeterminism[F] = F

  ////

  ////
}
