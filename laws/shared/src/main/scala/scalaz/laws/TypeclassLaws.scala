package scalaz
package laws

import scala.{ inline, Boolean }

import data._, tc._
import Scalaz._

object StrongLaws {
  // Nested `first` applications introducing multiple arguments
  // can be replaced by a single `first` application
  // introducing a tuple-full of arguments.
  @inline
  def firstAssoc[P[_, _], A, B, C, T](
    in: P[A, A]
  )(assert: (P[((A, B), C), ((A, B), C)], P[((A, B), C), ((A, B), C)]) => T)(implicit P: Strong[P]): T =
    assert(
      P.first(P.first(in)),
      P.dimap[(A, (B, C)), (A, (B, C)), ((A, B), C), ((A, B), C)](
        P.first(in)
      ) {
        case ((a, b), c) => (a, (b, c))
      } {
        case (a, (b, c)) => ((a, b), c)
      }
    )
}

object ChoiceLaws {
  // Nested `leftchoice` applications introducing multiple cases
  // can be replaced by a single `leftchoice` application
  // introducing an `\/`-full of cases.
  @inline
  def leftAssoc[P[_, _], A, B, C, T](
    in: P[A, A]
  )(assert: (P[(A \/ B) \/ C, (A \/ B) \/ C], P[(A \/ B) \/ C, (A \/ B) \/ C]) => T)(implicit P: Choice[P]): T =
    assert(
      P.leftchoice(P.leftchoice(in)),
      P.dimap[A \/ (B \/ C), A \/ (B \/ C), (A \/ B) \/ C, (A \/ B) \/ C](
        P.leftchoice(in)
      ) {
        case -\/(-\/(a)) => -\/(a)
        case -\/(\/-(b)) => \/-(-\/(b))
        case \/-(c)      => \/-(\/-(c))
      } {
        case -\/(a)      => -\/(-\/(a))
        case \/-(-\/(b)) => -\/(\/-(b))
        case \/-(\/-(c)) => \/-(c)
      }
    )
}

object ApplyLaws {
  // doing some reversed function composition doesn't break
  // code using `Apply`.
  @inline
  def applyAssoc[F[_], A, B, C, T](in: F[A])(fst: F[A => B],
                                             snd: F[B => C])(assert: (F[C], F[C]) => T)(implicit F: Apply[F]): T = {
    import F.{ ap, map }
    def compose(f: B => C)(g: A => B): A => C = f compose g
    assert(
      ap(ap(in)(fst))(snd),
      ap(in)(ap(fst)(map(snd)(compose)))
    )
  }
}

object ApplicativeLaws {
  // Combining with `F.pure` doesn't mutate the `F[_]` context.
  // Note that we don't need a left-handed and right-handed variation
  // of this law because the `Apply` composition law guarantees
  // that reversed function application doesn't change meaning.
  @inline
  def applyIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Applicative[F]) =
    assert(in, F.ap(in)(F.pure((a: A) => a)))
}

object BindLaws {
  @inline
  def bindAssoc[F[_], A, B, C, T](in: F[A])(fst: A => F[B],
                                            snd: B => F[C])(assert: (F[C], F[C]) => T)(implicit F: Bind[F]): T = {
    import F.flatMap
    assert(
      flatMap(flatMap(in)(fst))(snd),
      flatMap(in)(b => flatMap(fst(b))(snd))
    )
  }
}

object MonadLaws {
  @inline
  def bindIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Monad[F]) =
    assert(in, F.flatMap(in)(F.pure))
}

object CobindLaws {
  @inline
  def cobindAssoc[F[_], A, B, C, T](in: F[A])(fst: F[A] => B,
                                              snd: F[B] => C)(assert: (F[C], F[C]) => T)(implicit F: Cobind[F]): T = {
    import F.cobind
    assert(
      cobind(cobind(in)(fst))(snd),
      cobind(in)((b: F[A]) => snd(cobind(b)(fst)))
    )
  }
}

object ComonadLaws {
  @inline
  def cobindIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Comonad[F]): T =
    assert(in, F.cobind(in)(F.copoint))
}

object TraversableLaws {
  // it's okay to fuse multiple `traverse` calls
  // that can be in different applicatives
  // into a single call over the composite applicative.
  @inline
  def traverseComposition[T[_]: Traversable, F[_]: Applicative, G[_]: Applicative, A, B, C, Test](
    in: T[A]
  )(f: A => F[B], g: B => G[C])(assert: (F[G[T[C]]], F[G[T[C]]]) => Test): Test =
    assert(
      in.traverse(f).map(_.traverse(g)),
      Compose.run(in.traverse(a => Compose(f(a).map(g))))
    )

  @inline
  def traverseIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Traversable[F]) =
    assert(in, Identity.run(F.traverse(in)(Identity(_))))
}

object SemicategoryLaws {
  @inline
  def composeAssoc[F[_, _], A, B, C, D, T](fst: F[C, D], snd: F[B, C], thd: F[A, B])(
    assert: (F[A, D], F[A, D]) => T
  )(implicit F: Semicategory[F]): T = {
    import F.compose
    assert(
      compose(compose(fst, snd), thd),
      compose(fst, compose(snd, thd))
    )
  }
}

object CategoryLaws {
  @inline
  def composeRightIdentity[F[_, _], A, B, T](in: F[A, B])(assert: (F[A, B], F[A, B]) => T)(implicit F: Category[F]): T =
    assert(in, F.compose(in, F.id))

  @inline
  def composeLeftIdentity[F[_, _], A, B, T](in: F[A, B])(assert: (F[A, B], F[A, B]) => T)(implicit F: Category[F]): T =
    assert(in, F.compose(F.id, in))
}

object EqLaws {
  @inline
  def reflexivity[A, T](in: A)(assert: Boolean => T)(implicit A: Eq[A]): T =
    assert(A.equal(in, in))
}

object OrdLaws {

  @inline
  def antisymmetry[A, T](fst: A, snd: A)(assert: Boolean => T)(implicit A: Ord[A]): T = {
    val mustBeEqual =
      ((A.<=(fst, snd)) && (A.<=(snd, fst)))
    val satisfiesAntisymmetry =
      mustBeEqual == A.equal(fst, snd)
    assert(satisfiesAntisymmetry)
  }

  @inline
  def connex[A, T](fst: A, snd: A)(assert: Boolean => T)(implicit A: Ord[A]): T = {
    val satisfiesConnex = A.<=(fst, snd) || A.<=(snd, fst)
    assert(satisfiesConnex)
  }

  @inline
  def transitivity[A, T](fst: A, snd: A, thd: A)(assert: Boolean => T)(implicit A: Ord[A]): T = {
    val chain = A.<=(fst, snd) && A.<=(snd, thd)
    val satisfiesTransitivity =
      if (chain) A.<=(fst, thd)
      else true
    assert(satisfiesTransitivity)
  }

}

object MonoidLaws {
  @inline
  def leftIdentity[A, T](in: A)(assert: (A, A) => T)(implicit A: Monoid[A]): T =
    assert(in, A.mappend(A.mempty, in))

  @inline
  def rightIdentity[A, T](in: A)(assert: (A, A) => T)(implicit A: Monoid[A]): T =
    assert(in, A.mappend(in, A.mempty))
}

object SemigroupLaws {
  @inline
  def assoc[A, T](fst: A, snd: A, thd: A)(assert: (A, A) => T)(implicit A: Semigroup[A]): T = {
    import A.mappend
    assert(
      mappend(fst, mappend(snd, thd)),
      mappend(mappend(fst, snd), thd)
    )
  }
}
