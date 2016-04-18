package scalaz

/** `F` on the left, and `G` on the right, of [[scalaz.\/]].
  *
  * @param run The underlying [[scalaz.\/]]. */
final case class Coproduct[F[_], G[_], A](run: F[A] \/ G[A]) {
  import Coproduct._

  def map[B](f: A => B)(implicit F: Functor[F], G: Functor[G]): Coproduct[F, G, B] =
    Coproduct(run.bimap(F.lift(f), G.lift(f)))

  def cobind[B](f: Coproduct[F, G, A] => B)(implicit F: Cobind[F], G: Cobind[G]): Coproduct[F, G, B] =
    Coproduct(
      run.bimap(a => F.cobind(a)(x => f(leftc(x))), a => G.cobind(a)(x => f(rightc(x))))
    )

  def duplicate(implicit F: Cobind[F], G: Cobind[G]): Coproduct[F, G, Coproduct[F, G, A]] =
    Coproduct(run.bimap(
      x => F.extend(x)(a => leftc(a))
    , x => G.extend(x)(a => rightc(a)))
    )

  def copoint(implicit F: Comonad[F], G: Comonad[G]): A =
    run.fold(F.copoint(_), G.copoint(_))

  def contramap[B](f: B => A)(implicit F: Contravariant[F], G: Contravariant[G]): Coproduct[F, G, B] =
    Coproduct(run.bimap(F.contramap(_)(f), G.contramap(_)(f)))

  def fold[H[_]](f: F ~> H, g: G ~> H): H[A] =
    run.fold(f, g)

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F], G: Foldable[G]): Z =
    run.fold(a => F.foldRight(a, z)(f), a => G.foldRight(a, z)(f))

  def foldMap[B](f: A => B)(implicit F: Foldable[F], G: Foldable[G], M: Monoid[B]): B =
    run.fold(F.foldMap(_)(f), G.foldMap(_)(f))

  def foldMap1[B](f: A => B)(implicit F: Foldable1[F], G: Foldable1[G], M: Semigroup[B]): B =
    run.fold(F.foldMap1(_)(f), G.foldMap1(_)(f))

  def foldMapRight1[B](z: A => B)(f: (A, => B) => B)(implicit F: Foldable1[F], G: Foldable1[G]): B =
    run.fold(F.foldMapRight1(_)(z)(f), G.foldMapRight1(_)(z)(f))

  def traverse[X[_], B](g: A => X[B])(implicit F: Traverse[F], G: Traverse[G], A: Applicative[X]): X[Coproduct[F, G, B]] =
    run.fold(
      x => A.map(F.traverse(x)(g))(leftc(_))
    , x => A.map(G.traverse(x)(g))(rightc(_))
    )

  def traverse1[X[_], B](g: A => X[B])(implicit F: Traverse1[F], G: Traverse1[G], A: Apply[X]): X[Coproduct[F, G, B]] =
    run.fold(
      x => A.map(F.traverse1(x)(g))(leftc(_))
    , x => A.map(G.traverse1(x)(g))(rightc(_))
    )

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def unary_~ : Coproduct[G, F, A] =
    Coproduct(~run)

  def validation: Validation[F[A], G[A]] =
    run.validation

}

object Coproduct extends CoproductInstances {
  implicit def coproductTraverse1[F[_], G[_]](implicit F0: Traverse1[F], G0: Traverse1[G]): Traverse1[Coproduct[F, G, ?]] =
    new CoproductTraverse1[F, G] {
      override def F = F0
      override def G = G0
    }

  import Isomorphism._

  def coproductIso[F[_], G[_]]: Coproduct[F, G, ?] <~> λ[A => F[A] \/ G[A]] =
    new IsoFunctorTemplate[Coproduct[F, G, ?], λ[A => F[A] \/ G[A]]] {
      def from[A](ga: F[A] \/ G[A]) = Coproduct(ga)
      def to[A](fa: Coproduct[F, G, A]) = fa.run
    }

  def leftc[F[_], G[_], A](x: F[A]): Coproduct[F, G, A] =
    Coproduct(-\/(x))

  def rightc[F[_], G[_], A](x: G[A]): Coproduct[F, G, A] =
    Coproduct(\/-(x))

  final class CoproductLeft[G[_]] private[Coproduct]{
    def apply[F[_], A](fa: F[A]): Coproduct[F, G, A] = Coproduct(-\/(fa))
  }

  final class CoproductRight[F[_]] private[Coproduct]{
    def apply[G[_], A](ga: G[A]): Coproduct[F, G, A] = Coproduct(\/-(ga))
  }

  /** Like `Coproduct.leftc`, but specify only the `G`
   * @example {{{
   * Coproduct.left[Option](List(1)) // Coproduct[List, Option, Int](-\/(List(1)))
   * }}}
   */
  def left[G[_]]: CoproductLeft[G] = new CoproductLeft[G]

  /** Like `Coproduct.rightc`, but specify only the `F` */
  def right[F[_]]: CoproductRight[F] = new CoproductRight[F]
}

sealed abstract class CoproductInstances3 {
  type TupleCoglorified[F[_], G[_], A] =
  Coproduct[F, G, A]

  implicit def coproductEqual[F[_], G[_], A](implicit E: Equal[F[A] \/ G[A]]): Equal[Coproduct[F, G, A]] =
    Equal.equalBy(_.run)

  implicit def coproductFunctor[F[_], G[_]](implicit F0: Functor[F], G0: Functor[G]): Functor[Coproduct[F, G, ?]] = 
    new CoproductFunctor[F, G] {
      implicit def F: Functor[F] = F0
      implicit def G: Functor[G] = G0
    }

  implicit def coproductFoldable[F[_], G[_]](implicit F0: Foldable[F], G0: Foldable[G]): Foldable[Coproduct[F, G, ?]] = 
    new CoproductFoldable[F, G] {
      implicit def F: Foldable[F] = F0
      implicit def G: Foldable[G] = G0
    }
}

sealed abstract class CoproductInstances2 extends CoproductInstances3 {
  implicit def coproductContravariant[F[_], G[_]](implicit F0: Contravariant[F], G0: Contravariant[G]): Contravariant[Coproduct[F, G, ?]] = 
    new CoproductContravariant[F, G] {
      implicit def F: Contravariant[F] = F0
      implicit def G: Contravariant[G] = G0
    }

  implicit def coproductFoldable1[F[_], G[_]](implicit F0: Foldable1[F], G0: Foldable1[G]): Foldable1[Coproduct[F, G, ?]] =
    new CoproductFoldable1[F, G] {
      override def F = F0
      override def G = G0
    }
}

sealed abstract class CoproductInstances1 extends CoproductInstances2 {
  implicit def coproductCobind[F[_], G[_]](implicit F0: Cobind[F], G0: Cobind[G]): Cobind[Coproduct[F, G, ?]] = 
    new CoproductCobind[F, G] {
      implicit def F: Cobind[F] = F0
      implicit def G: Cobind[G] = G0
    }
}

sealed abstract class CoproductInstances0 extends CoproductInstances1 {
  implicit def coproductTraverse[F[_], G[_]](implicit F0: Traverse[F], G0: Traverse[G]): Traverse[Coproduct[F, G, ?]] =
    new CoproductTraverse[F, G] {
      implicit def F: Traverse[F] = F0
      implicit def G: Traverse[G] = G0
    }
}

sealed abstract class CoproductInstances extends CoproductInstances0 {
  implicit def coproductComonad[F[_], G[_]](implicit F0: Comonad[F], G0: Comonad[G]): Comonad[Coproduct[F, G, ?]] =
    new CoproductComonad[F, G] {
      implicit def F: Comonad[F] = F0
      implicit def G: Comonad[G] = G0
    }
}

private trait CoproductFunctor[F[_], G[_]] extends Functor[Coproduct[F, G, ?]] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f
}

private trait CoproductContravariant[F[_], G[_]] extends Contravariant[Coproduct[F, G, ?]] {
  implicit def F: Contravariant[F]
  implicit def G: Contravariant[G]

  override def contramap[A, B](a: Coproduct[F, G, A])(f: B => A) =
    a contramap f
}

private trait CoproductFoldable[F[_], G[_]] extends Foldable[Coproduct[F, G, ?]] {
  implicit def F: Foldable[F]
  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: Coproduct[F, G, A], z: => B)(f: (A, => B) => B): B =
    fa.foldRight(z)(f)

  override def foldMap[A, B](fa: Coproduct[F, G, A])(f: A => B)(implicit M: Monoid[B]) =
    fa foldMap f
}

private trait CoproductFoldable1[F[_], G[_]] extends Foldable1[Coproduct[F, G, ?]] {
  implicit def F: Foldable1[F]
  implicit def G: Foldable1[G]

  override final def foldMap1[A, B: Semigroup](fa: Coproduct[F, G, A])(f: A => B): B =
    fa.foldMap1(f)

  override final def foldMapRight1[A, B](fa: Coproduct[F, G, A])(z: A => B)(f: (A, => B) => B): B =
    fa.foldMapRight1(z)(f)
}

private trait CoproductTraverse[F[_], G[_]] extends Traverse[Coproduct[F, G, ?]] {
  implicit def F: Traverse[F]
  implicit def G: Traverse[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f

  override def traverseImpl[X[_]:Applicative,A,B](fa: Coproduct[F, G, A])(f: A => X[B]): X[Coproduct[F, G, B]] =
    fa traverse f
}

private trait CoproductTraverse1[F[_], G[_]] extends Traverse1[Coproduct[F, G, ?]] with CoproductFoldable1[F, G] {
  implicit def F: Traverse1[F]
  implicit def G: Traverse1[G]

  override final def traverse1Impl[X[_]: Apply, A, B](fa: Coproduct[F, G, A])(f: A => X[B]): X[Coproduct[F, G, B]] =
    fa traverse1 f

  override final def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f
}

private trait CoproductCobind[F[_], G[_]] extends Cobind[Coproduct[F, G, ?]] {
  implicit def F: Cobind[F]
  implicit def G: Cobind[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f

  override def cobind[A, B](a: Coproduct[F, G, A])(f: Coproduct[F, G, A] => B) =
    a cobind f

}

private trait CoproductComonad[F[_], G[_]] extends Comonad[Coproduct[F, G, ?]] {
  implicit def F: Comonad[F]
  implicit def G: Comonad[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f

  override def copoint[A](p: Coproduct[F, G, A]) =
    p.copoint

  override def cobind[A, B](a: Coproduct[F, G, A])(f: Coproduct[F, G, A] => B) =
    a cobind f

  override def cojoin[A](a: Coproduct[F, G, A]) =
    a.duplicate
}
