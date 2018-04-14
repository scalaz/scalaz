package scalaz

////
import scala.annotation.tailrec

/**
 * [[scalaz.Applicative]] without `point`.
 *
 * @see [[scalaz.Apply.ApplyLaw]]
 */
////
trait Apply[F[_]] extends Functor[F] with ApplyDivide[F] { self =>
  ////
  /** Sequence `f`, then `fa`, combining their results by function
    * application.
    *
    * NB: with respect to `apply2` and all other combinators, as well
    * as [[scalaz.Bind]], the `f` action appears to the *left*.  So
    * `f` should be the "first" `F`-action to perform.  This is in
    * accordance with all other implementations of this typeclass in
    * common use, which are "function first".
    */
  def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]

  // derived functions

  def traverse1[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse1[G]): F[G[B]] =
    G.traverse1(value)(f)(this)

  def sequence1[A, G[_]: Traverse1](as: G[F[A]]): F[G[A]] =
    traverse1(as)(a => a)

  /**
   * Repeats an applicative action infinitely
   */
  def forever[A, B](fa: F[A]): F[B] = discardLeft(fa, forever(fa))

  /**
   * Unfold `seed` to the right and combine effects left-to-right,
   * using the given [[Reducer]] to combine values.
   * Implementations may override this method to not unfold more
   * than is necessary to determine the result.
   */
  def unfoldrOpt[S, A, B](seed: S)(f: S => Maybe[(F[A], S)])(implicit R: Reducer[A, B]): Maybe[F[B]] = {
    @tailrec def go(acc: F[B], s: S): F[B] = f(s) match {
      case Maybe.Just((fa, s)) => go(apply2(acc, fa)(R.snoc), s)
      case _ => acc
    }
    f(seed) map { case (fa, s) => go(map(fa)(R.unit), s) }
  }

  /**The composition of Applys `F` and `G`, `[x]F[G[x]]`, is a Apply */
  def compose[G[_]](implicit G0: Apply[G]): Apply[λ[α => F[G[α]]]] =
    new CompositionApply[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Applys `F` and `G`, `[x](F[x], G[x]])`, is a Apply */
  def product[G[_]](implicit G0: Apply[G]): Apply[λ[α => (F[α], G[α])]] =
    new ProductApply[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /** An `Apply` for `F` in which effects happen in the opposite order. */
  def flip: Apply[F] = new FlippedApply {}

  protected[this] trait FlippedApply extends Apply[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      self.map(fa)(f)
    def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B] =
      self.ap(f)(self.map(fa)(a => (f: A => B) => f(a)))
    override def flip: self.type = self
  }

  /** Flipped variant of `ap`. */
  def apF[A,B](f: => F[A => B]): F[A] => F[B] = ap(_)(f)

  def ap2[A,B,C](fa: => F[A], fb: => F[B])(f: F[(A,B) => C]): F[C] =
    ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap3[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A,B,C) => D]): F[D] =
    ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
  def ap4[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: F[(A,B,C,D) => E]): F[E] =
    ap(fd)(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried)))))
  def ap5[A,B,C,D,E,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: F[(A,B,C,D,E) => R]): F[R] =
    ap(fe)(ap(fd)(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))))
  def ap6[A,B,C,D,E,FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: F[(A,B,C,D,E,FF) => R]): F[R] =
    ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried)))))))
  def ap7[A,B,C,D,E,FF,G,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: F[(A,B,C,D,E,FF,G) => R]): F[R] =
    ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))))))
  def ap8[A,B,C,D,E,FF,G,H,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: F[(A,B,C,D,E,FF,G,H) => R]): F[R] =
    ap(fh)(ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried)))))))))

  def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    ap(fb)(map(fa)(f.curried))
  def apply3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    ap(fc)(ap(fb)(map(fa)(f.curried)))
  def apply4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] =
    ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))
  def apply5[A, B, C, D, E, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: (A, B, C, D, E) => R): F[R] =
    ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried)))))
  def apply6[A, B, C, D, E, FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: (A, B, C, D, E, FF) => R): F[R] =
    ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))))
  def apply7[A, B, C, D, E, FF, G, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: (A, B, C, D, E, FF, G) => R): F[R] =
    ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried)))))))
  def apply8[A, B, C, D, E, FF, G, H, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: (A, B, C, D, E, FF, G, H) => R): F[R] =
    ap(fh)(ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))))))
  def apply9[A, B, C, D, E, FF, G, H, I, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I])(f: (A, B, C, D, E, FF, G, H, I) => R): F[R] =
    ap(fi)(ap(fh)(ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried)))))))))
  def apply10[A, B, C, D, E, FF, G, H, I, J, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J])(f: (A, B, C, D, E, FF, G, H, I, J) => R): F[R] =
    ap(fj)(ap(fi)(ap(fh)(ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))))))))
  def apply11[A, B, C, D, E, FF, G, H, I, J, K, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K])(f: (A, B, C, D, E, FF, G, H, I, J, K) => R): F[R] =
    ap(fk)(ap(fj)(ap(fi)(ap(fh)(ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried)))))))))))
  def apply12[A, B, C, D, E, FF, G, H, I, J, K, L, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L])(f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): F[R] =
    ap(fl)(ap(fk)(ap(fj)(ap(fi)(ap(fh)(ap(fg)(ap(ff)(ap(fe)(ap(fd)(ap(fc)(ap(fb)(map(fa)(f.curried))))))))))))

  final def applying1[Z, A1](f: A1 => Z)(
    implicit a1: F[A1]
  ): F[Z] = map(a1)(f)
  final def applying2[Z, A1, A2](
    f: (A1, A2) => Z
  )(implicit a1: F[A1], a2: F[A2]): F[Z] =
    apply2(a1, a2)(f)
  final def applying3[Z, A1, A2, A3](
    f: (A1, A2, A3) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] =
    apply3(a1, a2, a3)(f)
  final def applying4[Z, A1, A2, A3, A4](
    f: (A1, A2, A3, A4) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] =
    apply4(a1, a2, a3, a4)(f)

  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z, g: Z => (A1, A2)
  ): F[Z] = apply2(a1, a2)(f)
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = apply3(a1, a2, a3)(f)
  override def xproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = apply4(a1, a2, a3, a4)(f)

  def tuple2[A,B](fa: => F[A], fb: => F[B]): F[(A,B)] =
    apply2(fa, fb)((_,_))
  def tuple3[A,B,C](fa: => F[A], fb: => F[B], fc: => F[C]): F[(A,B,C)] =
    apply3(fa, fb, fc)((_,_,_))
  def tuple4[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D]): F[(A,B,C,D)] =
    apply4(fa, fb, fc, fd)((_,_,_,_))
  def tuple5[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E]): F[(A,B,C,D,E)] =
    apply5(fa, fb, fc, fd, fe)((_,_,_,_,_))

  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] =
    apply2(_, _)(f)
  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] =
    apply3(_, _, _)(f)
  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] =
    apply4(_, _, _, _)(f)
  def lift5[A, B, C, D, E, R](f: (A, B, C, D, E) => R): (F[A], F[B], F[C], F[D], F[E]) => F[R] =
    apply5(_, _, _, _, _)(f)
  def lift6[A, B, C, D, E, FF, R](f: (A, B, C, D, E, FF) => R): (F[A], F[B], F[C], F[D], F[E], F[FF]) => F[R] =
    apply6(_, _, _, _, _, _)(f)
  def lift7[A, B, C, D, E, FF, G, R](f: (A, B, C, D, E, FF, G) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G]) => F[R] =
    apply7(_, _, _, _, _, _, _)(f)
  def lift8[A, B, C, D, E, FF, G, H, R](f: (A, B, C, D, E, FF, G, H) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H]) => F[R] =
    apply8(_, _, _, _, _, _, _, _)(f)
  def lift9[A, B, C, D, E, FF, G, H, I, R](f: (A, B, C, D, E, FF, G, H, I) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I]) => F[R] =
    apply9(_, _, _, _, _, _, _, _, _)(f)
  def lift10[A, B, C, D, E, FF, G, H, I, J, R](f: (A, B, C, D, E, FF, G, H, I, J) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J]) => F[R] =
    apply10(_, _, _, _, _, _, _, _, _, _)(f)
  def lift11[A, B, C, D, E, FF, G, H, I, J, K, R](f: (A, B, C, D, E, FF, G, H, I, J, K) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K]) => F[R] =
    apply11(_, _, _, _, _, _, _, _, _, _, _)(f)
  def lift12[A, B, C, D, E, FF, G, H, I, J, K, L, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L]) => F[R] =
    apply12(_, _, _, _, _, _, _, _, _, _, _, _)(f)

  /** Combine `fa` and `fb` according to `Apply[F]` with a function that discards the `A`(s) */
  def discardLeft[A, B](fa: => F[A], fb: => F[B]): F[B] = apply2(fa,fb)((_,b) => b)

  /** Combine `fa` and `fb` according to `Apply[F]` with a function that discards the `B`(s) */
  def discardRight[A, B](fa: => F[A], fb: => F[B]): F[A] = apply2(fa,fb)((a,_) => a)

  /** Add a unit to any Apply to form an Applicative. */
  def applyApplicative: Applicative[λ[α => F[α] \/ α]] =
    new Applicative[λ[α => F[α] \/ α]] {
      // transliterated from semigroupoids 3.0.2, thanks edwardk
      def point[A](a: => A) = \/-(a)
      def ap[A, B](a: => F[A] \/ A)(f: => F[A => B] \/ (A => B)) = (f, a) match {
        case (\/-(f), \/-(a)) => \/-(f(a))
        case (\/-(f), -\/(a)) => -\/(self.map(a)(f))
        case (-\/(f), \/-(a)) => -\/(self.map(f)(_(a)))
        case (-\/(f), -\/(a)) => -\/(self.ap(a)(f))
      }
    }

  def liftReducer[A, B](implicit r: Reducer[A, B]): Reducer[F[A], F[B]] =
    new Reducer[F[A], F[B]] {
      def semigroup: Semigroup[F[B]] = Semigroup.liftSemigroup(Apply.this, r.semigroup)
      def unit(fa: F[A]): F[B] = map(fa)(r.unit)
      def cons(fa: F[A], fb: F[B]): F[B] = apply2(fa, fb)(r.cons)
      def snoc(fb: F[B], fa: F[A]): F[B] = apply2(fb, fa)(r.snoc)

      override def unfoldrOpt[S](seed: S)(f: S => Maybe[(F[A], S)]): Maybe[F[B]] =
        Apply.this.unfoldrOpt(seed)(f)
    }

  trait ApplyLaw extends FunctorLaw {
    /** Lifted functions can be fused. */
    def composition[A, B, C](fbc: F[B => C], fab: F[A => B], fa: F[A])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(ap(ap(fa)(fab))(fbc),
               ap(fa)(ap(fab)(map(fbc)((bc: B => C) => (ab: A => B) => bc compose ab))))
  }
  def applyLaw = new ApplyLaw {}

  ////
  val applySyntax = new scalaz.syntax.ApplySyntax[F] { def F = Apply.this }
}

object Apply {
  @inline def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  ////

  ////
}
