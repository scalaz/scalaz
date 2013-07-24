package scalaz

/** `F` on the left, and `G` on the right, of [[scalaz.\/]]. */
sealed trait Coproduct[F[_], G[_], A] {
  /** The underlying [[scalaz.\/]]. */
  val run: F[A] \/ G[A]

  import Coproduct._

  def map[B](f: A => B)(implicit F: Functor[F], G: Functor[G]): Coproduct[F, G, B] =
    Coproduct(run.bimap(F.map(_)(f), G.map(_)(f)))

  @deprecated("Each/foreach is deprecated", "7.1")
  def foreach(f: A => Unit)(implicit F: Each[F], G: Each[G]): Unit =
    run.fold(F.each(_)(f), G.each(_)(f))

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

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F], G: Foldable[G]): Z =
    run.fold(a => F.foldRight(a, z)(f), a => G.foldRight(a, z)(f))

  def foldMap[B](f: A => B)(implicit F: Foldable[F], G: Foldable[G], M: Monoid[B]): B =
    run.fold(F.foldMap(_)(f), G.foldMap(_)(f))

  def traverse[X[_], B](g: A => X[B])(implicit F: Traverse[F], G: Traverse[G], A: Applicative[X]): X[Coproduct[F, G, B]] =
    run.fold(
      x => A.map(F.traverse(x)(g))(leftc(_))
    , x => A.map(G.traverse(x)(g))(rightc(_))
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

object Coproduct extends CoproductFunctions with CoproductInstances0 {
  def apply[F[_], G[_], A](x: F[A] \/ G[A]): Coproduct[F, G, A] =
    new Coproduct[F, G, A] {
      val run = x
    }
}

trait CoproductFunctions {
  def leftc[F[_], G[_], A](x: F[A]): Coproduct[F, G, A] =
    Coproduct(\/.left(x))

  def rightc[F[_], G[_], A](x: G[A]): Coproduct[F, G, A] =
    Coproduct(\/.right(x))
}

trait CoproductInstances {
  type TupleCoglorified[F[_], G[_], A] =
  Coproduct[F, G, A]

  implicit def coproductFunctor[F[_], G[_]](implicit F0: Functor[F], G0: Functor[G]): Functor[({type λ[α]=Coproduct[F, G, α]})#λ] = new CoproductFunctor[F, G] {
    implicit def F: Functor[F] = F0
    implicit def G: Functor[G] = G0
  }

  implicit def coproductContravariant[F[_], G[_]](implicit F0: Contravariant[F], G0: Contravariant[G]): Contravariant[({type λ[α]=Coproduct[F, G, α]})#λ] = new CoproductContravariant[F, G] {
    implicit def F: Contravariant[F] = F0
    implicit def G: Contravariant[G] = G0
  }

  implicit def coproductFoldable[F[_], G[_]](implicit F0: Foldable[F], G0: Foldable[G]): Foldable[({type λ[α]=Coproduct[F, G, α]})#λ] = new CoproductFoldable[F, G] {
    implicit def F: Foldable[F] = F0
    implicit def G: Foldable[G] = G0
  }
}

trait CoproductInstances0 extends CoproductInstances {
  implicit def coproductCobind[F[_], G[_]](implicit F0: Cobind[F], G0: Cobind[G]): Cobind[({type λ[α]=Coproduct[F, G, α]})#λ] = new CoproductCobind[F, G] {
    implicit def F: Cobind[F] = F0
    implicit def G: Cobind[G] = G0
  }

  implicit def coproductTraverse[F[_], G[_]](implicit F0: Traverse[F], G0: Traverse[G]): Traverse[({type λ[α]=Coproduct[F, G, α]})#λ] = new CoproductTraverse[F, G] {
    implicit def F: Traverse[F] = F0
    implicit def G: Traverse[G] = G0
  }
}

trait CoproductInstances1 extends CoproductInstances0 {
  implicit def coproductComonad[F[_], G[_]](implicit F0: Comonad[F], G0: Comonad[G]): Comonad[({type λ[α]=Coproduct[F, G, α]})#λ] = new CoproductComonad[F, G] {
    implicit def F: Comonad[F] = F0
    implicit def G: Comonad[G] = G0
  }
}

private[scalaz] trait CoproductFunctor[F[_], G[_]] extends Functor[({type λ[α]=Coproduct[F, G, α]})#λ] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f
}

private[scalaz] trait CoproductContravariant[F[_], G[_]] extends Contravariant[({type λ[α]=Coproduct[F, G, α]})#λ] {
  implicit def F: Contravariant[F]
  implicit def G: Contravariant[G]

  override def contramap[A, B](a: Coproduct[F, G, A])(f: B => A) =
    a contramap f
}

private[scalaz] trait CoproductFoldable[F[_], G[_]] extends Foldable[({type λ[α]=Coproduct[F, G, α]})#λ] {
  implicit def F: Foldable[F]
  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: Coproduct[F, G, A], z: => B)(f: (A, => B) => B): B =
    fa.foldRight(z)(f)

  override def foldMap[A, B](fa: Coproduct[F, G, A])(f: A => B)(implicit M: Monoid[B]) =
    fa foldMap f
}

private[scalaz] trait CoproductTraverse[F[_], G[_]] extends Traverse[({type λ[α]=Coproduct[F, G, α]})#λ] {
  implicit def F: Traverse[F]
  implicit def G: Traverse[G]

  override def traverseImpl[X[_]:Applicative,A,B](fa: Coproduct[F, G, A])(f: A => X[B]): X[Coproduct[F, G, B]] =
    fa.run.fold(
      x => implicitly[Functor[X]].map(F.traverse(x)(f))(Coproduct.leftc(_))
    , x => implicitly[Functor[X]].map(G.traverse(x)(f))(Coproduct.rightc(_))
    )
}

private[scalaz] trait CoproductCobind[F[_], G[_]] extends Cobind[({type λ[α]=Coproduct[F, G, α]})#λ] {
  implicit def F: Cobind[F]
  implicit def G: Cobind[G]

  override def map[A, B](a: Coproduct[F, G, A])(f: A => B) =
    a map f

  override def cobind[A, B](a: Coproduct[F, G, A])(f: Coproduct[F, G, A] => B) =
    a cobind f

}

private[scalaz] trait CoproductComonad[F[_], G[_]] extends Comonad[({type λ[α]=Coproduct[F, G, α]})#λ] {
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
