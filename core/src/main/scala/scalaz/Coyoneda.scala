package scalaz

/**
 * The dual view of the Yoneda lemma. Also a free functor on `F`.
 * This is isomorphic to `F` as long as `F` itself is a functor.
 * The homomorphism from `F[A]` to `Coyoneda[F,A]` exists even when
 * `F` is not a functor.
 */
abstract class Coyoneda[F[_], A] { coyo =>
  type I
  def fi: F[I]
  def k(i: I): A

  /** Converts to `F[A]` given that `F` is a functor */
  def run(implicit F: Functor[F]): F[A] =
    F.map(fi)(k)

  /** Converts to `Yoneda[F,A]` given that `F` is a functor */
  def toYoneda(implicit F: Functor[F]): Yoneda[F, A] = new Yoneda[F, A] {
    def apply[B](f: A => B) = F.map(fi)(k _ andThen f)
  }

  /** Simple function composition. Allows map fusion without touching the underlying `F`. */
  def map[B](f: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
    type I = coyo.I
    val fi = coyo.fi
    def k(i: I) = f(coyo k i)
  }

  def trans[G[_]](f: F ~> G): Coyoneda[G, A] = new Coyoneda[G, A] {
    type I = coyo.I
    val fi = f(coyo.fi)
    def k(i: I) = coyo k i
  }

  import Id._

  /** `Coyoneda[F,_]` is the left Kan extension of `F` along `Id` */
  def toLan: Lan[Id, F, A] = new Lan[Id, F, A] {
    type I = coyo.I
    val v = fi
    def f(i: I) = k(i)
  }
}

object Coyoneda extends CoyonedaInstances {

  /** `F[A]` converts to `Coyoneda[F,A]` for any `F` */
  def apply[F[_],A](fa: F[A]): Coyoneda[F, A] = new Coyoneda[F, A] {
    type I = A
    def k(a: A) = a
    val fi = fa
  }

  type CoyonedaF[F[_]] = ({type A[α] = Coyoneda[F, α]})

  import Isomorphism._

  def iso[F[_]: Functor]: CoyonedaF[F]#A <~> F =
    new IsoFunctorTemplate[CoyonedaF[F]#A, F] {
      def from[A](fa: F[A]) = Coyoneda(fa)
      def to[A](fa: Coyoneda[F, A]) = fa.run
    }

  /** Turns a natural transformation F ~> G into CF ~> G */
  def liftTF[F[_], G[_]: Functor](fg: F ~> G): CoyonedaF[F]#A ~> G = {
    type CF[A] = Coyoneda[F, A]
    type CG[A] = Coyoneda[G, A]
    val m: (CF ~> CG) = liftT(fg)
    val n: (CG ~> G) = iso[G].to
    n compose m
  }

  /** Turns a natural transformation F ~> G into CF ~> CG */
  def liftT[F[_], G[_]](fg: F ~> G): CoyonedaF[F]#A ~> CoyonedaF[G]#A =
    new (CoyonedaF[F]#A ~> CoyonedaF[G]#A) {
      def apply[A](c: Coyoneda[F, A]) = c.trans(fg)
    }

}

sealed abstract class CoyonedaInstances extends CoyonedaInstances0 {
  implicit def coyonedaOrder[A, F[_]](implicit A: Order[F[A]], F: Functor[F]): Order[Coyoneda[F, A]] =
    new IsomorphismOrder[Coyoneda[F, A], F[A]] {
      def G = A
      def iso = Coyoneda.iso[F].unlift
    }
}

sealed abstract class CoyonedaInstances0 extends CoyonedaInstances1 {
  implicit def coyonedaComonad[F[_]: Comonad]: Comonad[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismComonad[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances1 extends CoyonedaInstances2 {
  implicit def coyonedaEqual[A, F[_]](implicit A: Equal[F[A]], F: Functor[F]): Equal[Coyoneda[F, A]] =
    new IsomorphismEqual[Coyoneda[F, A], F[A]] {
      def G = A
      def iso = Coyoneda.iso[F].unlift
    }

  implicit def coyonedaCobind[F[_]: Cobind]: Cobind[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismCobind[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances2 extends CoyonedaInstances3 {
  implicit def coyonedaTraverse1[F[_]: Traverse1]: Traverse1[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismTraverse1[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances3 extends CoyonedaInstances4 {
  implicit def coyonedaMonadPlus[F[_]: MonadPlus]: MonadPlus[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismMonadPlus[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances4 extends CoyonedaInstances5 {
  implicit def coyonedaApplicativePlus[F[_]: ApplicativePlus]: ApplicativePlus[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismApplicativePlus[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances5 extends CoyonedaInstances6 {
  implicit def coyonedaMonad[F[_]: Monad]: Monad[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismMonad[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }

  implicit def coyonedaPlusEmpty[F[_]: PlusEmpty: Functor]: PlusEmpty[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismEmpty[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances6 extends CoyonedaInstances7 {
  implicit def coyonedaBind[F[_]: Bind]: Bind[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismBind[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }

  implicit def coyonedaPlus[F[_]: Plus: Functor]: Plus[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismPlus[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances7 extends CoyonedaInstances8 {
  implicit def coyonedaApplicative[F[_]: Applicative]: Applicative[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismApplicative[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances8 extends CoyonedaInstances9 {
  implicit def coyonedaFoldable1[F[_]: Foldable1]: Foldable1[({type λ[α] = Coyoneda[F, α]})#λ] =
    new CoyonedaFoldable1[F]{ def F = implicitly }

  implicit def coyonedaApply[F[_]: Apply]: Apply[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismApply[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances9 extends CoyonedaInstances10 {
  implicit def coyonedaTraverse[F[_]: Traverse]: Traverse[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismTraverse[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }

  implicit def coyonedaContravariant[F[_]: Contravariant: Functor]: Contravariant[({type λ[α] = Coyoneda[F, α]})#λ] =
    new IsomorphismContravariant[({type λ[α] = Coyoneda[F, α]})#λ, F] {
      def G = implicitly
      def iso = Coyoneda.iso
    }
}

sealed abstract class CoyonedaInstances10 {
  implicit def coyonedaFoldable[F[_]: Foldable]: Foldable[({type λ[α] = Coyoneda[F, α]})#λ] =
    new CoyonedaFoldable[F]{ def F = implicitly }

  /** `Coyoneda[F,_]` is a functor for any `F` */
  implicit def coyonedaFunctor[F[_]]: Functor[({type λ[α] = Coyoneda[F, α]})#λ] =
    new Functor[({type λ[α] = Coyoneda[F,α]})#λ] {
      def map[A, B](ya: Coyoneda[F, A])(f: A => B) = ya map f
    }
}

private trait CoyonedaFoldable[F[_]] extends Foldable[({type λ[α] = Coyoneda[F, α]})#λ] {
  def F: Foldable[F]

  override final def foldMap[A, B: Monoid](fa: Coyoneda[F, A])(f: A => B) =
    F.foldMap(fa.fi)(i => f(fa.k(i)))
  override final def foldRight[A, B](fa: Coyoneda[F, A], z: => B)(f: (A, => B) => B) =
    F.foldRight(fa.fi, z)((i, b) => f(fa.k(i), b))
  override final def foldLeft[A, B](fa: Coyoneda[F, A], z: B)(f: (B, A) => B) =
    F.foldLeft(fa.fi, z)((b, i) => f(b, fa.k(i)))
}

private abstract class CoyonedaFoldable1[F[_]] extends Foldable1[({type λ[α] = Coyoneda[F, α]})#λ] with CoyonedaFoldable[F] {
  def F: Foldable1[F]

  override final def foldMap1[A, B: Semigroup](fa: Coyoneda[F, A])(f: A => B) =
    F.foldMap1(fa.fi)(i => f(fa.k(i)))
  override final def foldMapRight1[A, B](fa: Coyoneda[F, A])(z: A => B)(f: (A, => B) => B) =
    F.foldMapRight1(fa.fi)(i => z(fa.k(i)))((i, b) => f(fa.k(i), b))
}

