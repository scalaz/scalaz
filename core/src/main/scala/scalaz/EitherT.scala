package scalaz

/**
 * Represents a computation of type `F[Either[A, B]]`.
 *
 * Example:
 * {{{
 * val x: Option[Either[String, Int]] = Some(Right(1))
 * EitherT(x).map(1+).run // Some(Right(2)
 * }}}
 * */
sealed trait EitherT[F[_], A, B] {
  def run: F[Either[A, B]]

  import EitherT._
  import OptionT._

  def ?[X](left: => X, right: => X)(implicit F: Functor[F]): F[X] =
    F.map(run)((_: Either[A, B]).fold(_ => left, _ => right))

  def isLeft(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  def swap(implicit F: Functor[F]): F[Either[B, A]] =
    F.map(run)(_.swap)

  def getOrElse(default: => B)(implicit F: Functor[F]): F[B] =
    F.map(run)(_.right getOrElse default)

  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.right exists f)

  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.right forall f)

  def orElse(x: => EitherT[F, A, B])(implicit F: Bind[F]): EitherT[F, A, B] = {
    val g = run
    EitherT(F.bind(g) {
      case Left(_) => x.run
      case Right(_) => g
    })
  }

  def toOption(implicit F: Functor[F]): OptionT[F, B] =
    optionT[F](F.map(run)((_: Either[A, B]).right toOption))

  def toList(implicit F: Functor[F]): F[List[B]] =
    F.map(run)(_.fold(_ => Nil, List(_)))

  def toStream(implicit F: Functor[F]): F[Stream[B]] =
    F.map(run)((_: Either[A, B]).fold(_ => Stream(), Stream(_)))

  def map[C](f: B => C)(implicit F: Functor[F]): EitherT[F, A, C] =
    eitherT(F.map(run)(_.right.map(f)))

  def foreach(f: B => Unit)(implicit F: Each[F]): Unit =
    F.each(run)(_.right foreach f)

  def flatMap[C](f: B => EitherT[F, A, C])(implicit F: Monad[F]): EitherT[F, A, C] =
    eitherT(F.bind(run)(_.fold(a => F.point(Left(a): Either[A, C]), b => f(b).run)))

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    map(g).left.map(f)

  def bitraverse[G[_], C, D](f: (A) => G[C], g: (B) => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[F, C, D]] = {
    import std.either.eitherInstance
    Applicative[G].map(F.traverse(run)(Bitraverse[Either].bitraverseF(f, g)))(EitherT.eitherT(_: F[Either[C, D]]))
  }

  def traverse[G[_], C](f: (B) => G[C])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[F, A, C]] = {
    import std.either._
    G.map(F.traverse(run)(o => Traverse[({type λ[α] = Either[A, α]})#λ].traverse(o)(f)))(EitherT.eitherT(_))
  }

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z = {
    import std.either._
    F.foldRight[Either[A, B], Z](run, z)((a, b) => eitherMonad[A].foldRight[B, Z](a, b)(f))
  }

  def ap[C](f: => EitherT[F, A, B => C])(implicit F: Apply[F]): EitherT[F, A, C] = {
    import std.either._

    EitherT.eitherT[F, A, C](F.map2(f.run, run)((ff: Either[A, B => C], aa: Either[A, B]) => eitherMonad[A].ap(aa)(ff)))
  }

  def left: LeftProjectionT[F, A, B] = new LeftProjectionT[F, A, B]() {
    val eitherT = EitherT.this
  }

  def cozip(implicit Z: Cozip[F]): Either[F[A], F[B]] =
    Z.cozipT(this)
}

object EitherT extends EitherTFunctions with EitherTInstances {
  def apply[F[_], A, B](a: F[Either[A, B]]): EitherT[F, A, B] =
    eitherT[F, A, B](a)

  sealed trait LeftProjectionT[F[_], A, B] {
    def eitherT: EitherT[F, A, B]

    import OptionT._

    def getOrElse(default: => A)(implicit F: Functor[F]): F[A] =
      F.map(eitherT.run)(_.left getOrElse default)

    def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(eitherT.run)(_.left exists f)

    def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(eitherT.run)(_.left forall f)

    def orElse(x: => EitherT[F, A, B])(implicit m: Bind[F]): EitherT[F, A, B] = {
      val g = eitherT.run
      EitherT(m.bind(g){
        case Left(_) => g
        case Right(_) => x.run
      })
    }

    def toOption(implicit F: Functor[F]): OptionT[F, A] =
      optionT(F.map(eitherT.run)((_: Either[A, B]).left toOption))

    def toList(implicit F: Functor[F]): F[List[A]] =
      F.map(eitherT.run)(_.fold(List(_), _ => Nil))

    def toStream(implicit F: Functor[F]): F[Stream[A]] =
      F.map(eitherT.run)(_.fold(Stream(_), _ => Stream()))

    def map[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] =
      EitherT(F.map(eitherT.run)(_.left.map(f): Either[C, B]))

    def foreach(f: A => Unit)(implicit F: Each[F]): Unit =
      F.each(eitherT.run)(_.left foreach f)

    def flatMap[C](f: A => EitherT[F, C, B])(implicit F: Monad[F]): EitherT[F, C, B] =
      EitherT(F.bind(eitherT.run)(_.fold(a => f(a).run, b => F.point(Right(b): Either[C, B]))))
  }
}

trait EitherTInstances4 {
  implicit def eitherTFunctor[F[_], L](implicit F0: Functor[F]) = new EitherTFunctor[F, L] {
    implicit def F = F0
  }
}

trait EitherTInstances3 extends EitherTInstances4 {
  implicit def eitherTPointed[F[_], L](implicit F0: Pointed[F]) = new EitherTPointed[F, L] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionFunctor[F[_], L](implicit F0: Functor[F]) = new IsomorphismFunctor[({type λ[α] = EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α] = EitherT[F, L, α]})#λ] {
    implicit def G = eitherTFunctor[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }
}

trait EitherTInstances2 extends EitherTInstances3 {
  implicit def eitherTApply[F[_], L](implicit F0: Apply[F]) = new EitherTApply[F, L] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionPointed[F[_], L](implicit F0: Pointed[F]) = new IsomorphismPointed[({type λ[α] = EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α] = EitherT[F, L, α]})#λ] {
    implicit def G = eitherTPointed[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }
}

trait EitherTInstances1 extends EitherTInstances2 {
  implicit def eitherTApplicative[F[_], L](implicit F0: Applicative[F]) = new EitherTApplicative[F, L] {
    implicit def F = F0
  }

  implicit def eitherTLeftProjectionApplicative[F[_], L](implicit F0: Applicative[F]) = new IsomorphismApplicative[({type λ[α] = EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α] = EitherT[F, L, α]})#λ] {
    implicit def G = eitherTApplicative[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }  
}

trait EitherTInstances0 extends EitherTInstances1 {
  implicit def eitherTBifunctor[F[_]](implicit F0: Functor[F]) = new EitherTBifunctor[F] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionBifunctor[F[_]](implicit F0: Functor[F]) = new IsomorphismBifunctor[({type λ[α, β]=EitherT.LeftProjectionT[F, α, β]})#λ, ({type λ[α, β]=EitherT[F, α, β]})#λ] {
    implicit def G = eitherTBifunctor[F]
    def iso = EitherT.eitherTLeftProjectionIso2[F]
  }

  implicit def eitherTMonad[F[_], L](implicit F0: Monad[F]) = new EitherTMonad[F, L] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionMonad[F[_], L](implicit F0: Monad[F]) = new IsomorphismMonad[({type λ[α] = EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α] = EitherT[F, L, α]})#λ] {
    implicit def G = eitherTMonad[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }
  implicit def eitherTFoldable[F[_], L](implicit F0: Foldable[F]) = new EitherTFoldable[F, L] {
    implicit def F = F0
  }
}

// TODO more instances
trait EitherTInstances extends EitherTInstances0 {
  implicit def eitherTBitraverse[F[_]](implicit F0: Traverse[F]) = new EitherTBitraverse[F] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionBitraverse[F[_]](implicit F0: Traverse[F]) = new IsomorphismBitraverse[({type λ[α, β] = EitherT.LeftProjectionT[F, α, β]})#λ, ({type λ[α, β] = EitherT[F, α, β]})#λ] {
    implicit def G = eitherTBitraverse[F]
    def iso = EitherT.eitherTLeftProjectionIso2[F]
  }

  implicit def eitherTTraverse[F[_], L](implicit F0: Traverse[F]) = new EitherTTraverse[F, L] {
    implicit def F = F0
  }

  implicit def eitherTLeftProjectionTraverse[F[_], L](implicit F0: Traverse[F]) = new IsomorphismTraverse[({type λ[α]=EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α]=EitherT[F, L, α]})#λ] {
    implicit def G = eitherTTraverse[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }
  
  implicit def eitherTMonadTrans[A]: MonadTrans[({type λ[α[_], β] = EitherT[α, A, β]})#λ] = new EitherTMonadTrans[A] {}

  implicit def eitherTEqual[F[_], A, B](implicit F0: Equal[F[Either[A, B]]]): Equal[EitherT[F, A, B]] = F0.contramap((_: EitherT[F, A, B]).run)
}

trait EitherTFunctions {
  def eitherT[F[_], A, B](a: F[Either[A, B]]): EitherT[F, A, B] = new EitherT[F, A, B] {
    val run = a
  }

  type \/[A, B] =
  EitherT[Id, A, B]

  def leftT[F[_], A, B](a: A)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.point(Left(a): Either[A, B]))

  def rightT[F[_], A, B](b: B)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.point(Right(b): Either[A, B]))

  def fromEither[F[_], A, B](e: A \/ B)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.point(e.run))

  import Isomorphism.{IsoFunctorTemplate, IsoBifunctorTemplate}

  implicit def eitherTLeftProjectionEIso2[F[_], E] = new IsoFunctorTemplate[({type λ[α] = EitherT.LeftProjectionT[F, E, α]})#λ, ({type λ[α] = EitherT[F, E, α]})#λ] {
    def to[A](fa: EitherT.LeftProjectionT[F, E, A]): EitherT[F, E, A] = fa.eitherT
    def from[A](ga: EitherT[F, E, A]): EitherT.LeftProjectionT[F, E, A] = ga.left
  }
  implicit def eitherTLeftProjectionIso2[F[_]] = new IsoBifunctorTemplate[({type λ[α, β]=EitherT.LeftProjectionT[F, α, β]})#λ, ({type λ[α, β]=EitherT[F, α, β]})#λ] {
    def to[A, B](fa: EitherT.LeftProjectionT[F, A, B]): EitherT[F, A, B] = fa.eitherT
    def from[A, B](ga: EitherT[F, A, B]): EitherT.LeftProjectionT[F, A, B] = ga.left
  }
}

//
// Type class implementation traits
//

trait EitherTFunctor[F[_], E] extends Functor[({type λ[α]=EitherT[F, E, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: EitherT[F, E, A])(f: (A) => B): EitherT[F, E, B] = fa map f
}

trait EitherTPointed[F[_], E] extends Pointed[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTFunctor[F, E] {
  implicit def F: Pointed[F]

  def point[A](a: => A): EitherT[F, E, A] = EitherT.rightT(a)
}

trait EitherTApply[F[_], E] extends Apply[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTFunctor[F, E] {
  implicit def F: Apply[F]

  override def ap[A, B](fa: => EitherT[F, E, A])(f: => EitherT[F, E, (A) => B]): EitherT[F, E, B] = fa ap f
}

trait EitherTApplicative[F[_], E] extends Applicative[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTApply[F, E] with EitherTPointed[F, E] {
  implicit def F: Applicative[F]

  override def ap[A, B](fa: => EitherT[F, E, A])(f: => EitherT[F, E, (A) => B]): EitherT[F, E, B] = fa ap f
}

trait EitherTMonad[F[_], E] extends Monad[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTApplicative[F, E] {
  implicit def F: Monad[F]

  def bind[A, B](fa: EitherT[F, E, A])(f: (A) => EitherT[F, E, B]): EitherT[F, E, B] = fa flatMap f
}

trait EitherTFoldable[F[_], E] extends Foldable.FromFoldr[({type λ[α]=EitherT[F, E, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: EitherT[F, E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

trait EitherTTraverse[F[_], E] extends Traverse[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: EitherT[F, E, A])(f: (A) => G[B]): G[EitherT[F, E, B]] = fa traverse f
}

trait EitherTBifunctor[F[_]] extends Bifunctor[({type λ[α, β]=EitherT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: (A) => C, g: (B) => D): EitherT[F, C, D] = fab.bimap(f, g)
}

trait EitherTBitraverse[F[_]] extends Bitraverse[({type λ[α, β] = EitherT[F, α, β]})#λ] with EitherTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: EitherT[F, A, B])
                                                (f: (A) => G[C], g: (B) => G[D]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

trait EitherTMonadTrans[A] extends MonadTrans[({type λ[α[_], β] = EitherT[α, A, β]})#λ] {
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) = new (({type λ[α] = EitherT[M, A, α]})#λ ~> ({type λ[α] = EitherT[N, A, α]})#λ) {
    def apply[B](mb: EitherT[M, A, B]): EitherT[N, A, B] = EitherT(f.apply(mb.run))
  }

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): EitherT[M, A, B] = EitherT(M.map(mb)(Right[A, B](_)))

  implicit def apply[M[_] : Monad]: Monad[({type λ[α] = EitherT[M, A, α]})#λ] = EitherT.eitherTMonad
}
