package scalaz

////
/**
 *
 */
////
trait Bitraverse[F[_, _]] extends Bifunctor[F] with Bifoldable[F] { self =>
  ////
  def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  // derived functions

  /**The composition of Bitraverses `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bitraverse */
  def compose[G[_, _]](implicit G0: Bitraverse[G]): Bitraverse[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ] = new CompositionBitraverse[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  def bitraverseF[G[_] : Applicative, A, B, C, D](f: A => G[C], g: B => G[D]): F[A, B] => G[F[C, D]] =
    bitraverseImpl(_)(f, g)

  def bimap[A, B, C, D](fab: F[A, B])(f: (A) => C, g: (B) => D): F[C, D] = {
    bitraverseImpl[Id, A, B, C, D](fab)(f, g)
  }

  class Bitraversal[G[_]](implicit G: Applicative[G]) {
    def run[A,B,C,D](fa: F[A,B])(f: A => G[C])(g: B => G[D]): G[F[C, D]] = bitraverseImpl[G,A,B,C,D](fa)(f, g)
  }

  // reduce - given monoid
  def bitraversal[G[_]:Applicative]: Bitraversal[G] =
    new Bitraversal[G]

  def bitraversalS[S]: Bitraversal[({type f[x]=State[S,x]})#f] =
    new Bitraversal[({type f[x]=State[S,x]})#f]()(StateT.stateMonad)

  def bitraverse[G[_]:Applicative,A,B,C,D](fa: F[A,B])(f: A => G[C])(g: B => G[D]): G[F[C, D]] =
    bitraversal[G].run(fa)(f)(g)

  def bitraverseS[S,A,B,C,D](fa: F[A,B])(f: A => State[S,C])(g: B => State[S,D]): State[S,F[C, D]] =
    bitraversalS[S].run(fa)(f)(g)

  def runBitraverseS[S,A,B,C,D](fa: F[A,B], s: S)(f: A => State[S,C])(g: B => State[S,D]): (F[C, D], S) =
    bitraverseS(fa)(f)(g)(s)

  /** Bitraverse `fa` with a `State[S, G[C]]` and `State[S, G[D]]`, internally using a `Trampoline` to avoid stack overflow. */
  def traverseSTrampoline[S, G[_] : Applicative, A, B, C, D](fa: F[A, B])(f: A => State[S, G[C]])(g: B => State[S, G[D]]): State[S, G[F[C, D]]] = {
    import Free._
    implicit val A = StateT.stateTMonadState[S, Trampoline].compose(Applicative[G])
    bitraverse[({type λ[α]=StateT[Trampoline, S, G[α]]})#λ, A, B, C, D](fa)(f(_: A).lift[Trampoline])(g(_: B).lift[Trampoline]).unliftId[Trampoline]
  }

  /** Bitraverse `fa` with a `Kleisli[G, S, C]` and `Kleisli[G, S, D]`, internally using a `Trampoline` to avoid stack overflow. */
  def bitraverseKTrampoline[S, G[_] : Applicative, A, B, C, D](fa: F[A, B])(f: A => Kleisli[G, S, C])(g: B => Kleisli[G, S, D]): Kleisli[G, S, F[C, D]] = {
    import Free._
    implicit val A = Kleisli.kleisliMonadReader[Trampoline, S].compose(Applicative[G])
    Kleisli(bitraverse[({type λ[α]=Kleisli[Trampoline, S, G[α]]})#λ, A, B, C, D](fa)(z => Kleisli[Id, S, G[C]](i => f(z)(i)).lift[Trampoline])(z => Kleisli[Id, S, G[D]](i => g(z)(i)).lift[Trampoline]).unliftId[Trampoline] run _)
  }

  def bifoldLShape[A,B,C](fa: F[A,B], z: C)(f: (C,A) => C)(g: (C,B) => C): (F[Unit, Unit], C) =
    runBitraverseS(fa, z)(a => State(b => ((), f(b,a))))(b => State(c => ((), g(c,b))))

  def bisequence[G[_] : Applicative, A, B](x: F[G[A], G[B]]): G[F[A, B]] = bitraverseImpl(x)(fa => fa, fb => fb)

  override def bifoldLeft[A,B,C](fa: F[A, B], z: C)(f: (C, A) => C)(g: (C, B) => C): C =
    bifoldLShape(fa, z)(f)(g)._2

  def bifoldMap[A,B,M](fa: F[A, B])(f: A => M)(g: B => M)(implicit F: Monoid[M]): M =
    bifoldLShape(fa, F.zero)((m, a) => F.append(m, f(a)))((m, b) => F.append(m, g(b)))._2

  def bifoldRight[A,B,C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    bifoldMap(fa)((a: A) => (Endo.endo(f(a, _: C))))((b: B) => (Endo.endo(g(b, _: C)))) apply z

  ////
  val bitraverseSyntax = new scalaz.syntax.BitraverseSyntax[F] {}
}

object Bitraverse {
  @inline def apply[F[_, _]](implicit F: Bitraverse[F]): Bitraverse[F] = F

  ////

  ////
}

