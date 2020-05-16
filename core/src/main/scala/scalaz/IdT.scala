package scalaz

final case class IdT[F[_], A](run: F[A]) {
  def map[B](f: A => B)(implicit F: Functor[F]) =
    new IdT[F, B](F.map(run)(f))

  def flatMap[B](f: A => IdT[F, B])(implicit F: Bind[F]) =
    new IdT[F, B](F.bind(run)(f andThen ((_: IdT[F, B]).run)))

  def flatMapF[B](f: A => F[B])(implicit F: Bind[F]) =
    new IdT[F, B](F.bind(run)(f))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[A, Z](run, z)(f)

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[IdT[F, B]] =
    G.map(F.traverse(run)(f))(IdT(_))

  def ap[B](f: => IdT[F, A => B])(implicit F: Apply[F]) =
    new IdT(F.ap(run)(f.run))
}

sealed abstract class IdTInstances6 {
  implicit def idTDivisible[F[_]](implicit F0: Divisible[F]): Divisible[IdT[F, *]] =
    Divisible.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances5 extends IdTInstances6 {
  implicit def idTDecidable[F[_]](implicit F0: Decidable[F]): Decidable[IdT[F, *]] =
    Decidable.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances4 extends IdTInstances5 {
  implicit def idTFunctor[F[_]](implicit F0: Functor[F]): Functor[IdT[F, *]] =
    Functor.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances3 extends IdTInstances4 {
  implicit def idTApply[F[_]](implicit F0: Apply[F]): Apply[IdT[F, *]] =
    Apply.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances2 extends IdTInstances3 {
  implicit def idTApplicative[F[_]](implicit F0: Applicative[F]): Applicative[IdT[F, *]] =
    Applicative.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances1 extends IdTInstances2 {
  implicit def idTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[IdT[F, *]] =
    Foldable.fromIso(IdT.iso[F].to)

  implicit def idTEqual[F[_], A](implicit F: Equal[F[A]]): Equal[IdT[F, A]] =
    F.contramap(_.run)

  implicit def idTMonad[F[_]](implicit F0: Monad[F]): Monad[IdT[F, *]] =
    Monad.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances0 extends IdTInstances1 {
  implicit def idTOrder[F[_], A](implicit F: Order[F[A]]): Order[IdT[F, A]] =
    F.contramap(_.run)

  implicit def idTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[IdT[F, *]] =
    Traverse.fromIso(IdT.iso[F])
}

sealed abstract class IdTInstances extends IdTInstances0 {
  implicit val idTHoist: Hoist[IdT] = IdTHoist

  implicit def idTBindRec[F[_]](implicit F0: BindRec[F]): BindRec[IdT[F, *]] =
    BindRec.fromIso(IdT.iso[F])

  implicit val idTCohoist: Cohoist[IdT] =
    new Cohoist[IdT] {
      override def cohoist[M[_], N[_]: Comonad](f: M ~> N) =
        new (IdT[M, *] ~> IdT[N, *]){
          def apply[A](x: IdT[M, A]) = IdT(f(x.run))
        }

      override def lower[G[_]: Cobind, A](a: IdT[G, A]) =
        a.run
    }
}

object IdT extends IdTInstances {
  import Isomorphism._
  def iso[F[_]]: IdT[F, *] <~> F = new IsoFunctorTemplate[IdT[F, *], F] {
    def from_[A](ga: F[A]): IdT[F, A] = IdT[F, A](ga)
    def to_[A](fa: IdT[F, A]): F[A] = fa.run
  }
}

private object IdTHoist extends Hoist[IdT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): IdT[G, A] =
    new IdT[G, A](a)

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    new (IdT[M, *] ~> IdT[N, *]) {
      def apply[A](fa: IdT[M, A]): IdT[N, A] =
        new IdT[N, A](f(fa.run))
    }

  implicit def apply[G[_] : Monad]: Monad[IdT[G, *]] =
    IdT.idTMonad[G]
}

// vim: set ts=4 sw=4 et:
