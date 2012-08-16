package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
trait ApplyOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Apply[F]
  ////

  final def <*>[B](f: F[B]): F[(A,B)] = F.tuple(self,f)
  final def tuple[B](f: F[B]): F[(A,B)] = F.tuple(self,f)

  @deprecated("Use `a tuple b` instead", "7")
  final def pair[B](f: F[B]): F[(A,B)] = F.tuple(self,f)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that discards the `A`s */
  final def *>[B](fb: F[B]): F[B] = F(self,fb)((_,b) => b)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that discards the `B`s */
  final def <*[B](fb: F[B]): F[A] = F(self,fb)((a,_) => a)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that constructs a `Tuple2[A, B]` */
  @deprecated("Use `a <*> b` instead", "7")
  final def <|*|>[B](fb: F[B]): F[(A, B)] = F.tuple(self,fb)

  @deprecated("Use `on(f1,f2..fN)((a,b,c) => ..)` instead", "7")
  final def <**>[B, C](b: F[B])(f: (A, B) => C): F[C] = F(self, b)(f)
  @deprecated("Use `on(f1,f2..fN)((a,b,c) => ..)` instead", "7")
  final def <***>[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = F(self, fb, fc)(f)
  @deprecated("Use `on(f1,f2..fN)((a,b,c) => ..)` instead", "7")
  final def <****>[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = F(self, fb, fc, fd)(f)
  @deprecated("Use `on(f1,f2..fN)((a,b,c) => ..)` instead", "7")
  final def <*****>[B, C, D, E, F0](fb: F[B], fc: F[C], fd: F[D], e: F[E])(f: (A, B, C, D, E) => F0): F[F0] = F(self, fb, fc, fd, e)(f)

  /**
   * DSL for constructing Applicative expressions.
   *
   * `(f1 |@| f2 |@| ... |@| fn)((v1, v2, ... vn) => ...)` is an alternative to `Apply[F].mapN(f1, f2, ..., fn)((v1, v2, ... vn) => ...)`
   *
   * `(f1 |@| f2 |@| ... |@| fn).tupled` is an alternative to `Apply[F].mapN(f1, f2, ..., fn)(TupleN.apply _)`
   *
   * Warning: each call to `|@|` leads to an allocation of wrapper object. For performance sensitive code, consider using
   *          [[scalaz.Apply]]`#mapN` directly.
   */
  @deprecated("Use `on(f1,f2..fN)((a,b,c) => ..)` instead", "7")
  final def |@|[B](fb: F[B]) = new ApplicativeBuilder[F, A, B] {
    val a: F[A] = self
    val b: F[B] = fb
  }
  /** Alias for `|@|` */
  @deprecated("Use `on(f1,f2..fN)((a,b,c) => ..)` instead", "7")
  final def ⊛[B](fb: F[B]) = |@|(fb)

  ////
}

trait ToApplyOps0 {
  implicit def ToApplyOpsUnapply[FA](v: FA)(implicit F0: Unapply[Apply, FA]) =
    new ApplyOps[F0.M,F0.A] { def self = F0(v); implicit def F: Apply[F0.M] = F0.TC }

}

trait ToApplyOps extends ToApplyOps0 with ToFunctorOps {
  implicit def ToApplyOps[F[_],A](v: F[A])(implicit F0: Apply[F]) =
    new ApplyOps[F,A] { def self = v; implicit def F: Apply[F] = F0 }

  ////

  ////
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] { self => 
  implicit def ToApplyOps[A](v: F[A]): ApplyOps[F, A] = new ApplyOps[F,A] { def self = v; implicit def F: Apply[F] = self.F }

  def F: Apply[F]
  ////
  implicit def lift2[A,B,C](f: (A,B) => C) = F.lift2(f)
  implicit def lift3[A,B,C,D](f: (A,B,C) => D) = F.lift3(f)
  implicit def lift4[A,B,C,D,E](f: (A,B,C,D) => E) = F.lift4(f)

  def on[A,B,C](fa: => F[A], fb: => F[B])(
               f: (A, B) => C): F[C] =
    F(fa, fb)(f)

  def on[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(
                 f: (A, B, C) => D): F[D] =
    F(fa, fb, fc)(f)

  def on[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(
                   f: (A,B,C,D) => E): F[E] =
    F(fa, fb, fc, fd)(f)

  def on[A,B,C,D,E,I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
                     f: (A,B,C,D,E) => I): F[I] =
    F(fa, fb, fc, fd, fe)(f)

  def on[A,B,C,D,E,I,J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
                       f: (A,B,C,D,E,I) => J): F[J] =
    F(fa, fb, fc, fd, fe, fi)(f)

  def on[A,B,C,D,E,I,J,K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J])(
                         f: (A,B,C,D,E,I,J) => K): F[K] =
    F(fa, fb, fc, fd, fe, fi, fj)(f)

  ////
}
