package scalaz

import Isomorphism.{<~>}
import scalaz.EitherT.LeftProjectionT

sealed trait EitherT[F[_], A, B] {
  def runT: F[Either[A, B]]

  import EitherT._
  import OptionT._

  def run(implicit iso: F <~> Id): Either[A, B] =
    iso.to(runT)

  def ?[X](left: => X, right: => X)(implicit F: Functor[F]): F[X] =
    F.map(runT)((_: Either[A, B]).fold(_ => left, _ => right))

  def -?-[X](left: => X, right: => X)(implicit i: F <~> Id): X =
    run(i).fold(_ => left, _ => right)

  def isLeftT(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.isLeft)

  def isLeft(implicit i: F <~> Id): Boolean =
    run.isLeft

  def isRightT(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.isRight)

  def isRight(implicit i: F <~> Id): Boolean =
    run.isRight

  def swapT(implicit F: Functor[F]): F[Either[B, A]] =
    F.map(runT)(_.swap)

  def swap(implicit i: F <~> Id): Either[B, A] =
    run.swap

  def getOrElseT(default: => B)(implicit F: Functor[F]): F[B] =
    F.map(runT)(_.right getOrElse default)

  def getOrElse(default: => B)(implicit i: F <~> Id): B =
    run.right getOrElse default

  def existsT(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.right exists f)

  def exists(f: B => Boolean)(implicit i: F <~> Id): Boolean =
    run.right exists f

  def forallT(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.right forall f)

  def forall(f: B => Boolean)(implicit i: F <~> Id): Boolean =
    run.right forall f

  def orElse(x: => EitherT[F, A, B])(implicit F: Bind[F]): EitherT[F, A, B] = {
    val g = runT
    EitherT(F.bind(g) {
      case Left(_) => x.runT
      case Right(_) => g
    })
  }

  def toOptionT(implicit F: Functor[F]): OptionT[F, B] =
    optionT(F.map(runT)((_: Either[A, B]).right toOption))

  def toOption(implicit i: F <~> Id): Option[B] =
    run.right.toOption

  def toListT(implicit F: Functor[F]): F[List[B]] =
    F.map(runT)(_.fold(_ => Nil, List(_)))

  def toList(implicit i: F <~> Id): List[B] =
    run.fold(_ => Nil, List(_))

  def toStreamT(implicit F: Functor[F]): F[Stream[B]] =
    F.map(runT)((_: Either[A, B]).fold(_ => Stream(), Stream(_)))

  def toStream(implicit i: F <~> Id): Stream[B] =
    run.fold(_ => Stream(), Stream(_))

  def map[C](f: B => C)(implicit F: Functor[F]): EitherT[F, A, C] =
    eitherT(F.map(runT)(_.right.map(f)))

  def foreach(f: B => Unit)(implicit F: Each[F]): Unit =
    F.each(runT)(_.right foreach f)

  def flatMap[C](f: B => EitherT[F, A, C])(implicit F: Monad[F]): EitherT[F, A, C] =
    eitherT(F.bind(runT)(_.fold(a => F.point(Left(a): Either[A, C]), b => f(b).runT)))

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    map(g).left.map(f)

  def bitraverse[G[_], C, D](f: (A) => G[C], g: (B) => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[F, C, D]] = {
    import std.either.eitherInstance
    Applicative[G].map(F.traverse(runT)(BiTraverse[Either].bitraverseF(f, g)))(EitherT.eitherT(_: F[Either[C, D]]))
  }

  def traverse[G[_], C](f: (B) => G[C])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[F, A, C]] = {
    import std.either._
    G.map(F.traverse(runT)(o => Traverse[({type λ[α] = Either[A, α]})#λ].traverse(o)(f)))(EitherT.eitherT(_))
  }

  def foldRight[Z](z: Z)(f: (B) => (=> Z) => Z)(implicit F: Foldable[F]): Z = {
    import std.either._
    F.foldR[Either[A, B], Z](runT, z)(a => b => eitherMonad[A].foldR[B, Z](a, b)(f))
  }

  def ap[C](f: EitherT[F, A, B => C])(implicit F: Applicative[F]): EitherT[F, A, C] = {
    import std.either._

    EitherT.eitherT[F, A, C](F.lift2((ff: Either[A, B => C], aa: Either[A, B]) => eitherMonad[A].ap(aa)(ff))(f.runT, runT))
  }

  def left: LeftProjectionT[F, A, B] = new LeftProjectionT[F, A, B]() {
    val eitherT = EitherT.this
  }
}

object EitherT extends EitherTFunctions with EitherTInstances {
  def apply[F[_], A, B](a: F[Either[A, B]]): EitherT[F, A, B] =
    eitherT[F, A, B](a)

  sealed trait LeftProjectionT[F[_], A, B] {
    def eitherT: EitherT[F, A, B]

    import OptionT._

    def getOrElseT(default: => A)(implicit F: Functor[F]): F[A] =
      F.map(eitherT.runT)(_.left getOrElse default)

    def getOrElse(default: => A)(implicit i: F <~> Id): A =
      eitherT.run.left getOrElse default

    def existsT(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(eitherT.runT)(_.left exists f)

    def exists(f: A => Boolean)(implicit i: F <~> Id): Boolean =
      eitherT.run.left exists f

    def forallT(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(eitherT.runT)(_.left forall f)

    def forall(f: A => Boolean)(implicit i: F <~> Id): Boolean =
      eitherT.run.left forall f

    def orElse(x: => EitherT[F, A, B])(implicit m: Bind[F]): EitherT[F, A, B] = {
      val g = eitherT.runT
      EitherT(m.bind(g){
        case Left(_) => g
        case Right(_) => x.runT
      })
    }

    def toOptionT(implicit F: Functor[F]): OptionT[F, A] =
      optionT(F.map(eitherT.runT)((_: Either[A, B]).left toOption))

    def toOption(implicit i: F <~> Id): Option[A] =
      eitherT.run.left.toOption

    def toListT(implicit F: Functor[F]): F[List[A]] =
      F.map(eitherT.runT)(_.fold(List(_), _ => Nil))

    def toList(implicit i: F <~> Id): List[A] =
      eitherT.run.fold(List(_), _ => Nil)

    def toStreamT(implicit F: Functor[F]): F[Stream[A]] =
      F.map(eitherT.runT)(_.fold(Stream(_), _ => Stream()))

    def toStream(implicit i: F <~> Id): Stream[A] =
      eitherT.run.fold(Stream(_), _ => Stream())

    def map[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] =
      EitherT(F.map(eitherT.runT)(_.left.map(f): Either[C, B]))

    def foreach(f: A => Unit)(implicit F: Each[F]): Unit =
      F.each(eitherT.runT)(_.left foreach f)

    def flatMap[C](f: A => EitherT[F, C, B])(implicit F: Monad[F]): EitherT[F, C, B] =
      EitherT(F.bind(eitherT.runT)(_.fold(a => f(a).runT, b => F.point(Right(b): Either[C, B]))))
  }
}

trait EitherTInstances3 {
  implicit def eitherTFunctor[F[_], L](implicit F0: Functor[F]) = new EitherTFunctor[F, L] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionFunctor[F[_], L](implicit F0: Functor[F]) = new IsomorphismFunctor[({type λ[α] = EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α] = EitherT[F, L, α]})#λ] {
    implicit def G = eitherTFunctor[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }
}

trait EitherTInstances2 extends EitherTInstances3 {
  implicit def eitherTPointed[F[_], L](implicit F0: Pointed[F]) = new EitherTPointed[F, L] {
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
  implicit def eitherTBiFunctor[F[_]](implicit F0: Functor[F]) = new EitherTBiFunctor[F] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionBiFunctor[F[_]](implicit F0: Functor[F]) = new IsomorphismBiFunctor[({type λ[α, β]=EitherT.LeftProjectionT[F, α, β]})#λ, ({type λ[α, β]=EitherT[F, α, β]})#λ] {
    implicit def G = eitherTBiFunctor[F]
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
  implicit def eitherTBiTraverse[F[_]](implicit F0: Traverse[F]) = new EitherTBiTraverse[F] {
    implicit def F = F0
  }
  implicit def eitherTLeftProjectionBiTraverse[F[_]](implicit F0: Traverse[F]) = new IsomorphismBiTraverse[({type λ[α, β] = EitherT.LeftProjectionT[F, α, β]})#λ, ({type λ[α, β] = EitherT[F, α, β]})#λ] {
    implicit def G = eitherTBiTraverse[F]
    def iso = EitherT.eitherTLeftProjectionIso2[F]
  }

  implicit def eitherTTraverse[F[_], L](implicit F0: Traverse[F]) = new EitherTTraverse[F, L] {
    implicit def F = F0
  }

  implicit def eitherTLeftProjectionTraverse[F[_], L](implicit F0: Traverse[F]) = new IsomorphismTraverse[({type λ[α]=EitherT.LeftProjectionT[F, L, α]})#λ, ({type λ[α]=EitherT[F, L, α]})#λ] {
    implicit def G = eitherTTraverse[F, L]
    def iso = EitherT.eitherTLeftProjectionEIso2[F, L]
  }
}

trait EitherTFunctions {
  def eitherT[F[_], A, B](a: F[Either[A, B]]): EitherT[F, A, B] = new EitherT[F, A, B] {
    val runT = a
  }

  type \/[A, B] =
  EitherT[Id, A, B]

  def leftT[F[_], A, B](a: A)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.point(Left(a): Either[A, B]))

  def rightT[F[_], A, B](b: B)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.point(Right(b): Either[A, B]))

  def fromEither[F[_], A, B](e: A \/ B)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.point(e.runT))

  import Isomorphism.{IsoFunctorTemplate, IsoBiFunctorTemplate}

  implicit def eitherTLeftProjectionEIso2[F[_], E] = new IsoFunctorTemplate[({type λ[α] = EitherT.LeftProjectionT[F, E, α]})#λ, ({type λ[α] = EitherT[F, E, α]})#λ] {
    def to[A](fa: EitherT.LeftProjectionT[F, E, A]): EitherT[F, E, A] = fa.eitherT
    def from[A](ga: EitherT[F, E, A]): EitherT.LeftProjectionT[F, E, A] = ga.left
  }
  implicit def eitherTLeftProjectionIso2[F[_]] = new IsoBiFunctorTemplate[({type λ[α, β]=EitherT.LeftProjectionT[F, α, β]})#λ, ({type λ[α, β]=EitherT[F, α, β]})#λ] {
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

trait EitherTApplicative[F[_], E] extends Applicative[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTPointed[F, E] {
  implicit def F: Applicative[F]

  override def ap[A, B](fa: EitherT[F, E, A])(f: EitherT[F, E, (A) => B]): EitherT[F, E, B] = fa ap f
}

trait EitherTMonad[F[_], E] extends Monad[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTApplicative[F, E] {
  implicit def F: Monad[F]

  def bind[A, B](fa: EitherT[F, E, A])(f: (A) => EitherT[F, E, B]): EitherT[F, E, B] = fa flatMap f
}

trait EitherTFoldable[F[_], E] extends Foldable.FromFoldr[({type λ[α]=EitherT[F, E, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldR[A, B](fa: EitherT[F, E, A], z: B)(f: (A) => (=> B) => B): B = fa.foldRight(z)(f)
}

trait EitherTTraverse[F[_], E] extends Traverse[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: EitherT[F, E, A])(f: (A) => G[B]): G[EitherT[F, E, B]] = fa traverse f
}

trait EitherTBiFunctor[F[_]] extends BiFunctor[({type λ[α, β]=EitherT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: (A) => C, g: (B) => D): EitherT[F, C, D] = fab.bimap(f, g)
}

trait EitherTBiTraverse[F[_]] extends BiTraverse[({type λ[α, β] = EitherT[F, α, β]})#λ] with EitherTBiFunctor[F] {
  implicit def F: Traverse[F]

  def bitraverse[G[_] : Applicative, A, B, C, D](fab: EitherT[F, A, B])
                                                (f: (A) => G[C], g: (B) => G[D]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}