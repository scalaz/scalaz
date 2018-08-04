package scalaz
package laws

import scala.{ Nothing, Unit }

import data._, tc._, Scalaz._

object HomomorphismLaws {
  // To be a monoid homomorphism, a function must satisfy both `monoidIdentity`
  // and `semigroupAppend`.
  def monoidIdentity[A, B, T](f: A => B)(assert: (B, B) => T)(implicit A: Monoid[A], B: Monoid[B]): T =
    assert(f(A.mempty), B.mempty)

  def semigroupAppend[A, B, T](fst: A, snd: A)(f: A => B)(assert: (B, B) => T)(implicit A: Semigroup[A],
                                                                               B: Semigroup[B]): T =
    assert(B.mappend(f(fst), f(snd)), f(A.mappend(fst, snd)))

  // all natural transformations are Functor homomorphisms,
  // Contravariant homomorphisms,
  // InvariantFunctor homomorphisms,
  // and Phantom homomorphisms.

  // all binatural transformations are Bifunctor homomorphisms
  // and Profunctor homomorphisms.

  def applyAp[F[_], G[_], A, B, T](
    transform: F ~> G
  )(
    fa: F[A],
    ff: F[A => B]
  )(
    assert: (G[B], G[B]) => T
  )(
    implicit F: Apply[F],
    G: Apply[G]
  ): T =
    assert(
      transform.of[B](F.ap(fa)(ff)),
      G.ap(transform.of(fa))(transform.of(ff))
    )

  def applicativeIdentity[F[_], G[_], A, T](
    transform: F ~> G
  )(
    in: A
  )(
    assert: (G[A], G[A]) => T
  )(
    implicit F: Applicative[F],
    G: Applicative[G]
  ): T =
    assert(
      transform.of(F.pure(in)),
      G.pure(in)
    )

  def bindFlatMap[F[_], G[_], A, B, T](
    transform: F ~> G
  )(
    fa: F[A],
    f: A => F[B]
  )(
    assert: (G[B], G[B]) => T
  )(
    implicit F: Bind[F],
    G: Bind[G]
  ): T =
    assert(
      transform.of(F.flatMap(fa)(f)),
      G.flatMap(transform.of(fa))(a => transform.of(f(a)))
    )

  def monadIdentity[F[_], G[_], A, T](
    transform: F ~> G
  )(
    in: A
  )(
    assert: (G[A], G[A]) => T
  )(
    implicit F: Monad[F],
    G: Monad[G]
  ): T =
    applicativeIdentity[F, G, A, T](transform)(in)(assert)

  // the `G[A] => B` is odd, but it makes sense
  // because like the `bindFlatMap` check,
  // the `G[A] => B` is used to construct an
  // `F[A] => G[B]`.
  def cobindCoflatMap[F[_], G[_], A, B, T](
    transform: F ~> G
  )(
    fa: F[A],
    f: G[A] => B
  )(
    assert: (G[B], G[B]) => T
  )(
    implicit F: Cobind[F],
    G: Cobind[G]
  ): T =
    assert(
      transform.of(F.cobind(fa)(nfa => f(transform.of(nfa)))),
      G.cobind(transform.of(fa))(f)
    )

  def comonadIdentity[F[_], G[_], A, T](
    transform: F ~> G
  )(
    in: F[A]
  )(
    assert: (A, A) => T
  )(
    implicit F: Comonad[F],
    G: Comonad[G]
  ): T =
    assert(F.copoint(in), G.copoint(transform.of(in)))

  def semicategoryCompose[F[_, _], G[_, _], A, B, C, D, T](
    transform: F ~~> G
  )(
    fst: F[B, C],
    snd: F[A, B]
  )(
    assert: (G[A, C], G[A, C]) => T
  )(
    implicit F: Semicategory[F],
    G: Semicategory[G]
  ): T =
    assert(
      G.compose(transform.of(fst), transform.of(snd)),
      transform.of(F.compose(fst, snd))
    )

  def categoryIdentity[F[_, _], G[_, _], A, T](
    transform: F ~~> G
  )(
    assert: (G[A, A], G[A, A]) => T
  )(
    implicit F: Category[F],
    G: Category[G]
  ): T =
    assert(transform.of(F.id[A]), G.id[A])

  def strongFirst[F[_, _], G[_, _], A, B, C, T](
    transform: F ~~> G
  )(
    in: F[A, B]
  )(
    assert: (G[A, B], G[A, B]) => T
  )(
    implicit F: Strong[F],
    G: Strong[G]
  ): T = {
    def elimUnit[A](tuple: (A, Unit)): A = tuple._1
    def introUnit[A](a: A): (A, Unit)    = (a, ())
    assert(
      transform.of(
        F.dimap[(A, Unit), (B, Unit), A, B](
          F.first(in)
        ) { introUnit(_) } { elimUnit(_) }
      ),
      G.dimap[(A, Unit), (B, Unit), A, B](
        G.first(transform.of(in))
      ) { introUnit(_) } { elimUnit(_) }
    )
  }

  def choiceLeft[F[_, _], G[_, _], A, B, C, T](
    transform: F ~~> G
  )(
    in: F[A, B]
  )(
    assert: (G[A, B], G[A, B]) => T
  )(
    implicit F: Choice[F],
    G: Choice[G]
  ): T = {
    def elimNothing[A](either: A \/ Nothing): A = either.fold(a => a, n => n)
    def introNothing[A](a: A): A \/ Nothing     = -\/(a)
    assert(
      transform.of(
        F.dimap[A \/ Nothing, B \/ Nothing, A, B](
          F.leftchoice(in)
        ) { introNothing(_) } { elimNothing(_) }
      ),
      G.dimap[A \/ Nothing, B \/ Nothing, A, B](
        G.leftchoice(transform.of(in))
      ) { introNothing(_) } { elimNothing(_) }
    )
  }

}
