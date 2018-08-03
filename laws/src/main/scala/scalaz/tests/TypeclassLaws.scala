package scalaz
package laws

import scala.{ inline, Boolean }
import scala.Predef.identity

import data._, tc._

// the other "composition" laws for all kinds of functors are guaranteed by parametricity
// in addition to the "identity" law.
object FunctorLaws {
  @inline
  def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Functor[F]): T =
    assert(in, F.map(in)(identity))
}

object ContravariantLaws {
  @inline
  def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Contravariant[F]): T =
    assert(in, F.contramap(in)(identity))
}

object InvariantFunctorLaws {
  @inline
  def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: InvariantFunctor[F]): T =
    assert(in, F.imap(in)(identity)(identity))
}

object PhantomLaws {
  @inline
  def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Phantom[F]): T =
    assert(in, F.pmap(in))
}

object BifunctorLaws {
  @inline
  def bimapIdentityToIdentity[F[_, _], A, B, T](
    in: F[A, B]
  )(assert: (F[A, B], F[A, B]) => T)(implicit F: Bifunctor[F]): T =
    assert(in, F.bimap(in)(identity, identity))
}

object ProfunctorLaws {
  @inline
  def identityToIdentity[P[_, _], A, B, T](in: P[A, B])(assert: (P[A, B], P[A, B]) => T)(implicit P: Profunctor[P]): T =
    assert(
      in,
      P.dimap[A, B, A, B](in)(identity)(identity)
    )
}

object StrongLaws {
  // Nested `first` applications introducing multiple arguments
  // can be replaced by a single `first` application introducing a tuple-full of arguments.
  //
  // first' . first' ≡ dimap assoc unassoc . first' where
  // assoc ((a,b),c) = (a,(b,c))
  // unassoc (a,(b,c)) = ((a,b),c)
  @inline
  def firstAssoc[P[_, _], A, B, C, T]
    (in: P[A, A])
    (assert: (P[((A, B), C), ((A, B), C)], P[((A, B), C), ((A, B), C)]) => T)
    (implicit P: Strong[P]): T = {
    assert(
      P.first(P.first(in)),
      P.dimap[(A, (B, C)), (A, (B, C)), ((A, B), C), ((A, B), C)](
        P.first(in)
      ){
        case ((a, b), c) => (a, (b, c))
      } {
        case (a, (b, c)) => ((a, b), c)
      }
    )
  }
}

object ChoiceLaws {
  // Nested `leftchoice` applications introducing multiple cases
  // can be replaced by a single `leftchoice` application introducing an `\/`-full of cases.
  //
  // left' . left' ≡ dimap assocE unassocE . left' where
  // assocE :: Either (Either a b) c -> Either a (Either b c)
  // assocE (Left (Left a)) = Left a
  // assocE (Left (Right b)) = Right (Left b)
  // assocE (Right c) = Right (Right c)
  // unassocE :: Either a (Either b c) -> Either (Either a b) c
  // unassocE (Left a) = Left (Left a)
  // unassocE (Right (Left b) = Left (Right b)
  // unassocE (Right (Right c)) = Right c)
  @inline
  def leftAssoc[P[_, _], A, B, C, T]
    (in: P[A, A])
    (assert: (P[(A \/ B) \/ C, (A \/ B) \/ C], P[(A \/ B) \/ C, (A \/ B) \/ C]) => T)
    (implicit P: Choice[P]): T = {
    assert(
      P.leftchoice(P.leftchoice(in)),
      P.dimap[A \/ (B \/ C), A \/ (B \/ C), (A \/ B) \/ C, (A \/ B) \/ C](
        P.leftchoice(in)
      ){
        case -\/(-\/(a)) => -\/(a)
        case -\/(\/-(b)) => \/-(-\/(b))
        case \/-(c)      => \/-(\/-(c))
      } {
        case -\/(a)      => -\/(-\/(a))
        case \/-(-\/(a)) => -\/(\/-(a))
        case \/-(\/-(b)) => \/-(b)
      }
    )
  }
}

object ApplyLaws {
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
  @inline
  def applyIdentity[F[_], A, B, T](in: A)(f: A => B)(assert: (F[B], F[B]) => T)(implicit F: Applicative[F]) =
    assert(
      F.pure(f(in)),
      F.ap(F.pure(in))(F.pure(f))
    )
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

}

object ComonadLaws {

}

object TraverseLaws {
  @inline
  def traverse
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
  // pass me two `A`'s
  // and some way to observe an `A` as a `B`
  // and I'll tell you if the observations agree
  // with their equality.
  @inline
  def identity[A, B, T](fst: A, snd: A)(f: A => B)(assert: (Boolean, B, B) => T)(implicit A: Eq[A]): T =
    assert(A.equal(fst, snd), f(fst), f(snd))

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
