package scalaz

import Isomorphism.{<~>}

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
    eitherT(F.bind(runT)(_.fold(a => F.pure(Left(a): Either[A, C]), b => f(b).runT)))

  def left: LeftProjectionT[F, A, B] = new LeftProjectionT[F, A, B]() {
    val e = EitherT.this
  }

}

object EitherT extends EitherTFunctions with EitherTInstances {
  def apply[F[_], A, B](a: F[Either[A, B]]): EitherT[F, A, B] =
    eitherT[F, A, B](a)

  sealed trait LeftProjectionT[F[_], A, B] {
    def e: EitherT[F, A, B]

    import OptionT._

    def getOrElseT(default: => A)(implicit F: Functor[F]): F[A] =
      F.map(e.runT)(_.left getOrElse default)

    def getOrElse(default: => A)(implicit i: F <~> Id): A =
      e.run.left getOrElse default

    def existsT(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(e.runT)(_.left exists f)

    def exists(f: A => Boolean)(implicit i: F <~> Id): Boolean =
      e.run.left exists f

    def forallT(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(e.runT)(_.left forall f)

    def forall(f: A => Boolean)(implicit i: F <~> Id): Boolean =
      e.run.left forall f

    def orElse(x: => EitherT[F, A, B])(implicit m: Bind[F]): EitherT[F, A, B] = {
      val g = e.runT
      EitherT(m.bind(g){
        case Left(_) => g
        case Right(_) => x.runT
      })
    }

    def toOptionT(implicit F: Functor[F]): OptionT[F, A] =
      optionT(F.map(e.runT)((_: Either[A, B]).left toOption))

    def toOption(implicit i: F <~> Id): Option[A] =
      e.run.left.toOption

    def toListT(implicit F: Functor[F]): F[List[A]] =
      F.map(e.runT)((_: Either[A, B]).fold(List(_), _ => Nil))

    def toList(implicit i: F <~> Id): List[A] =
      e.run.fold(List(_), _ => Nil)

    def toStreamT(implicit F: Functor[F]): F[Stream[A]] =
      F.map(e.runT)(_.fold(Stream(_), _ => Stream()))

    def toStream(implicit i: F <~> Id): Stream[A] =
      e.run.fold(Stream(_), _ => Stream())

    def map[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] =
      eitherT(F.map(e.runT)(_.left.map(f): Either[C, B]))

    def foreach(f: A => Unit)(implicit F: Each[F]): Unit =
      F.each(e.runT)(_.left foreach f)

    def flatMap[C](f: A => EitherT[F, C, B])(implicit F: Monad[F]): EitherT[F, C, B] =
      eitherT(F.bind(e.runT)(_.fold(a => f(a).runT, b => F.pure(Right(b): Either[C, B]))))
  }
}

trait EitherTInstances0 {
  implicit def eitherTBiFunctor[F[_]](implicit F0: Functor[F]) = new EitherTBiFunctor[F] {
    implicit def F = F0
  }
}

// TODO more instances
trait EitherTInstances extends EitherTInstances0 {
  implicit def eitherTBiTraverse[F[_]](implicit F0: Traverse[F]) = new EitherTBiTraverse[F] {
    implicit def F = F0
  }
}

trait EitherTFunctions {
  def eitherT[F[_], A, B](a: F[Either[A, B]]): EitherT[F, A, B] = new EitherT[F, A, B] {
    val runT = a
  }

  type \/[A, B] =
  EitherT[Id, A, B]

  def leftT[F[_], A, B](a: A)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.pure(Left(a): Either[A, B]))

  def rightT[F[_], A, B](b: B)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.pure(Right(b): Either[A, B]))

  def fromEither[F[_], A, B](e: A \/ B)(implicit F: Pointed[F]): EitherT[F, A, B] =
    eitherT(F.pure(e.runT))
}

//
// Type class implementation traits
//
trait EitherTBiFunctor[F[_]] extends BiFunctor[({type λ[α, β]=EitherT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: (A) => C, g: (B) => D): EitherT[F, C, D] = fab.map(g).left.map(f)
}

trait EitherTBiTraverse[F[_]] extends BiTraverse[({type λ[α, β] = EitherT[F, α, β]})#λ] with EitherTBiFunctor[F] {
  implicit def F: Traverse[F]

  import std.either.eitherInstance

  def bitraverse[G[_] : Applicative, A, B, C, D](fab: EitherT[F, A, B])
                                                (f: (A) => G[C], g: (B) => G[D]): G[EitherT[F, C, D]] =
    Applicative[G].map(F.traverse(fab.runT)(BiTraverse[Either].bitraverseF(f, g)))(EitherT.eitherT(_: F[Either[C, D]]))
}